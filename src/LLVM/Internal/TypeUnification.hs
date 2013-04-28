{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- | This module defines a function to perform unification of LLVM types.
--
-- The unification step is intended to eliminate duplicate struct
-- definitions wherever possible.  Duplicate structs have .NNN
-- suffixes after their names and are constructed by the linker when
-- it cannot prove that types are equivalent.
--
-- In theory, llvm-link should do this (and it does try).  However,
-- the implementation in llvm-link is poor and gives up on even
-- trivial cases.  This implementation is a bit more robust and
-- sensitive to some frontend quirks.
--
-- This is a low-level implementation that is run at IR translation
-- time.  The result of 'unifyTypes' is a map from CType pointers to
-- Type objects.  This map can be used at IR construction time to map each
-- C type to a canonical high-level representation if at all possible.
--
--  * Opaque types are eliminated if there is a core of unifiable
--    definitions of types with the same name
--
--  * A canonical representative is chosen for each base type name
--
-- Moving this analysis to very early in the translation process has
-- two major benefits: 1) all higher-level analyses can do type-level
-- comparisons without ad-hoc normalization steps, and 2) the knot
-- tying process in the IR translation is simpler because types are
-- already fully resolved.
--
-- TODO: This could be made more configurable for more aggressive
-- unification (e.g., in the presence of {}* fields - this elimination is not
-- exactly safe)
module LLVM.Internal.TypeUnification (
  unifyTypes
  ) where

import Control.Applicative
import Control.Monad ( filterM, foldM, forM_, forM )
import Control.Monad.Trans.Class
import Control.Monad.Error.Class
import Control.Monad.EitherK
import Control.Unification
import Control.Unification.IntVar
import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )
import Data.Set ( Set )
import qualified Data.Set as S
import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as HM
import Data.HashTable.IO ( BasicHashTable )
import qualified Data.HashTable.IO as HT
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Monoid
import qualified Data.Text as T
import Data.Word ( Word64 )
import Debug.Trace.LocationTH
import Foreign.Ptr
import System.IO.Unsafe ( unsafePerformIO )

import Data.LLVM.Types
import LLVM.Internal.Interop

-- This is adapted from one of the tests in unification-fd

-- | A type of Type terms
data T a = T String [a]
         deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Unifiable T where
  zipMatch (T a xs) (T b ys) =
    case a == b of
      True ->
        case pair xs ys of
          Nothing -> Nothing
          Just p -> Just (T a (map Right p))
      False -> Nothing

type TTerm = UTerm T IntVar

-- | Build a term from a name and a list of term arguments.
typeTerm :: String -> [TTerm] -> TTerm
typeTerm = (UTerm .) . T

unifyTypes :: [TypePtr] -> IO (BasicHashTable Word64 Type, Map Type Int)
unifyTypes tptrs = do
  tmap <- HT.new
  smap <- HT.new

  -- Translate all non-struct types directly into the hash table.  The
  -- fold only accumulates struct types (including unions and classes) into
  -- a HashMap from name to ptr.
  --
  -- This accumulated map is the one that will be unified over.
  structsByName <- foldM (translateAndGroup tmap smap) mempty tptrs

  -- Now, for each name in structsByName, attempt to unify all of the
  -- types in the group.  For each type that does unify, make an entry
  -- in tmap for its pointer to the single representative Type.  For
  -- the rest, do a basic translation.
  mapM_ (unifyStructTypes tmap smap) (HM.toList structsByName)

  typeSizes <- HT.foldM (htToMap tmap) mempty smap

  return (tmap, typeSizes)

htToMap :: BasicHashTable Word64 Type
        -> Map Type Int
        -> (Word64, Int)
        -> IO (Map Type Int)
htToMap tmap a (ip, v) = do
  Just t <- HT.lookup tmap ip
  return $ M.insert t v a

unifyStructTypes :: BasicHashTable Word64 Type
                 -> BasicHashTable Word64 Int
                 -> (String, [(TypePtr, Int)])
                 -> IO ()
unifyStructTypes tmap smap (name, allPtrs) = do
  sizedPtrs <- filterM (isNotOpaque . fst) allPtrs
  let ptrs = map fst sizedPtrs
  -- All of the types reachable from the list of input types @ptrs@
  retainedTypes <- retainedTypeSearch ptrs

  -- The struct types that are reachable from the input types, grouped
  -- by their base names.
  namedStructs <- filterM isStruct (S.toList retainedTypes)
  let dependencyTypes = filter (`notElem` ptrs) namedStructs
  groupedDependencyTypes <- groupByBaseName dependencyTypes

  (unifyResult, _) <- runIntBindingT $ do
    -- Assign a logic variable to each struct type reachable from
    -- the input types (including the input types)
    varMap <- assignVars (HM.insert name ptrs groupedDependencyTypes)
    -- Convert everything to terms, then start unifying groups.
    -- The final unification gives us the answer we want.
    primaryTerms <- mapM (toTerm varMap) ptrs
    let dterms = map snd $ HM.toList groupedDependencyTypes
    depTermGroups <- forM dterms $ \dptrs -> do
      -- If a dependency is opaque, don't bother trying to unify it
      -- since it won't have the right arity.
      nonOpaqueDeps <- lift $ filterM isNotOpaque dptrs
      dts <- mapM (toTerm varMap) nonOpaqueDeps
      return dts
    runEitherKT $ do
      mapM_ unifies depTermGroups
      unifies primaryTerms
  case unifyResult of
    Left _ ->
      -- Make an entry in tmap for each input
      forM_ allPtrs $ \(p, sz) -> do
        Just name' <- cTypeName p
        isPacked <- cTypeIsPacked p
        mptrs <- cTypeList p
        let t = TypeStruct (Right (T.pack name')) (map (delayedLookup tmap) mptrs) isPacked
            ip = fromIntegral (ptrToIntPtr p)
        HT.insert tmap ip t
        HT.insert smap ip sz
    Right _ ->
      case sizedPtrs of
        [] -> do
          -- Make a single opaque entry for allPtrs
          let t = TypeStruct (Right (T.pack name)) [] False
          forM_ allPtrs $ \(p, sz) -> do
            let ip = fromIntegral (ptrToIntPtr p)
            HT.insert tmap ip t
            HT.insert smap ip sz
        (p0, _) : _ -> do
          -- Make one type to represent all of these types that were unified
          -- and use it as the entry for each of the input pointers.
          mptrs <- cTypeList p0
          isPacked <- cTypeIsPacked p0
          let t = TypeStruct (Right (T.pack name)) (map (delayedLookup tmap) mptrs) isPacked
          forM_ allPtrs $ \(p, sz) -> do
            let ip = fromIntegral (ptrToIntPtr p)
            HT.insert tmap ip t
            HT.insert smap ip sz

translateAndGroup :: BasicHashTable Word64 Type
                  -> BasicHashTable Word64 Int
                  -> HashMap String [(TypePtr, Int)]
                  -> TypePtr
                  -> IO (HashMap String [(TypePtr, Int)])
translateAndGroup tmap sizeMap structs tptr = do
  let ip = fromIntegral $ ptrToIntPtr tptr
  tag <- cTypeTag tptr
  byteSize <- cTypeSizeInBytes tptr
  HT.insert sizeMap ip byteSize
  case tag of
    TYPE_STRUCT -> do
      name <- cTypeName tptr
      case name of
        Just n -> return $ HM.insertWith (++) (structBaseName n) [(tptr, byteSize)] structs
        -- Anonymous struct
        Nothing -> do
          isPacked <- cTypeIsPacked tptr
          ptrs <- cTypeList tptr
          let ts = map (delayedLookup tmap) ptrs
              stype = TypeStruct (Left ip) ts isPacked
          HT.insert tmap ip stype
          return structs
    _ -> do
      t' <- case tag of
        TYPE_VOID -> return TypeVoid
        TYPE_FLOAT -> return TypeFloat
        TYPE_DOUBLE -> return TypeDouble
        TYPE_X86_FP80 -> return TypeX86FP80
        TYPE_FP128 -> return TypeFP128
        TYPE_PPC_FP128 -> return TypePPCFP128
        TYPE_LABEL -> return TypeLabel
        TYPE_METADATA -> return TypeMetadata
        TYPE_X86_MMX -> return TypeX86MMX
        TYPE_INTEGER -> do
          sz <- cTypeSize tptr
          return $ TypeInteger sz
        TYPE_FUNCTION -> do
          isVa <- cTypeIsVarArg tptr
          rtp <- cTypeInner tptr
          argTypePtrs <- cTypeList tptr

          let rt = delayedLookup tmap rtp
              argTs = map (delayedLookup tmap) argTypePtrs
          return $ TypeFunction rt argTs isVa
        TYPE_ARRAY -> do
          sz <- cTypeSize tptr
          itp <- cTypeInner tptr
          let innerType = delayedLookup tmap itp
          return $ TypeArray sz innerType
        TYPE_VECTOR -> do
          sz <- cTypeSize tptr
          itp <- cTypeInner tptr
          let innerType = delayedLookup tmap itp
          return $ TypeVector sz innerType
        TYPE_POINTER -> do
          itp <- cTypeInner tptr
          addrSpc <- cTypeAddrSpace tptr
          let innerType = delayedLookup tmap itp
          return $ TypePointer innerType addrSpc
        TYPE_STRUCT -> $failure "Impossible, cannot have a struct here"
      HT.insert tmap ip t'
      return structs

-- | Look up the Type associated with a TypePtr lazily and impurely.
-- This relies on no entries in the hash table being inspected until
-- everything is unified and all entries are made.
--
-- This is simpler, faster, and more space efficient than using real
-- knot tying.  It is also safe here since there are never any lookups
-- in the hash table in this algorithm - only the IR translator
-- actually performs lookups.
delayedLookup :: BasicHashTable Word64 Type -> TypePtr -> Type
delayedLookup tmap ptr = unsafePerformIO $ do
  res <- HT.lookup tmap $ fromIntegral (ptrToIntPtr ptr)
  return $ fromMaybe errMsg res
  where
    errMsg = $failure ("Type lookup failure for " ++ show ptr)

-- | Perform a breadth-first search to discover all types referenced
-- by the given input type list
retainedTypeSearch :: [TypePtr] -> IO (Set TypePtr)
retainedTypeSearch = go mempty . S.fromList
  where
    go visited q = do
      let vals = q `S.difference` visited
      case S.null vals of
        True -> return visited
        False -> do
          let visited' = visited `S.union` vals
          q' <- foldM addValuesFrom mempty (S.toList vals)
          go visited' q'

    addValuesFrom q tp = do
      tag <- cTypeTag tp
      case tag of
        TYPE_STRUCT -> do
          ptrs <- cTypeList tp
          return $ q `S.union` S.fromList ptrs
        TYPE_FUNCTION -> do
          rtp <- cTypeInner tp
          atps <- cTypeList tp
          return $ q `S.union` S.fromList (rtp : atps)
        TYPE_ARRAY -> do
          itp <- cTypeInner tp
          return $ S.insert itp q
        TYPE_VECTOR -> do
          itp <- cTypeInner tp
          return $ S.insert itp q
        TYPE_POINTER -> do
          itp <- cTypeInner tp
          return $ S.insert itp q
        _ -> return $ S.insert tp q

-- | Top level term creator; this only works for named structs.
toTerm :: Map TypePtr TTerm -> TypePtr -> IntBindingT T IO TTerm
toTerm varMap tp = do
  let Just svar = M.lookup tp varMap
  Just name <- lift $ cTypeName tp
  ptrs <- lift $ cTypeList tp
  innerTerms <- mapM (toTerm' varMap) ptrs
  return $ typeTerm (structBaseName name) (svar : innerTerms)

-- | Inner term creator; for structs, just return the variable
-- assigned to that Type.
toTerm' :: Map TypePtr TTerm -> TypePtr -> IntBindingT T IO TTerm
toTerm' varMap tp = do
  tag <- lift $ cTypeTag tp
  case tag of
    TYPE_VOID -> return $ typeTerm "%primitive.void" []
    TYPE_FLOAT -> return $ typeTerm "%primitive.float" []
    TYPE_DOUBLE -> return $ typeTerm "%primtive.double" []
    TYPE_X86_FP80 -> return $ typeTerm "%primitive.x86_fp80" []
    TYPE_FP128 -> return $ typeTerm "%primitive.fp128" []
    TYPE_PPC_FP128 -> return $ typeTerm "%primitive.ppc_fp128" []
    TYPE_LABEL -> return $ typeTerm "%primitive.label" []
    TYPE_METADATA -> return $ typeTerm "%primitive.metadata" []
    TYPE_X86_MMX -> return $ typeTerm "%primitive.x86_mmx" []
    TYPE_INTEGER -> do
      sz <- lift $ cTypeSize tp
      return $ typeTerm ("%primitive.i" ++ show sz) []
    TYPE_FUNCTION -> do
      rtp <- lift $ cTypeInner tp
      atps <- lift $ cTypeList tp
      innerTerms <- mapM (toTerm' varMap) (rtp : atps)
      return $ typeTerm "%primitive.function" innerTerms
    TYPE_ARRAY -> do
      sz <- lift $ cTypeSize tp
      itp <- lift $ cTypeInner tp
      ity <- toTerm' varMap itp
      return $ typeTerm ("%primitive.array." ++ show sz) [ity]
    TYPE_VECTOR -> do
      sz <- lift $ cTypeSize tp
      itp <- lift $ cTypeInner tp
      ity <- toTerm' varMap itp
      return $ typeTerm ("%primitive.vector." ++ show sz) [ity]
    TYPE_POINTER -> do
      -- If the struct is an opaque literal ptr {}*, assign it a fresh
      -- variable (we will allow such an opaque ref to be anything
      -- since the frontend seems to put them in strange places).
      itp <- lift $ cTypeInner tp
      innerTag <- lift $ cTypeTag itp
      case innerTag of
        TYPE_STRUCT -> do
          mptrs <- lift $ cTypeList itp
          case null mptrs of
            True -> UVar <$> freeVar
            False -> do
              ity <- toTerm' varMap itp
              return $ typeTerm "*" [ity]
        _ -> do
          ity <- toTerm' varMap itp
          return $ typeTerm "*" [ity]
    TYPE_STRUCT ->
      case M.lookup tp varMap of
        Nothing -> do
          mptrs <- lift $ cTypeList tp
          innerTerms <- mapM (toTerm' varMap) mptrs
          return $ typeTerm "%anon.struct" innerTerms
        Just v -> return v

-- | Assign fresh unification variables to every struct type in the
-- input map.
assignVars :: HashMap String [TypePtr] -> IntBindingT T IO (Map TypePtr TTerm)
assignVars = foldM assignVarL mempty . HM.toList
  where
    assignVarL acc (_, ts) = foldM assignVar acc ts
    assignVar acc ty = do
      x <- freeVar
      return $! M.insert ty (UVar x) acc

-- | Take a list of types and group them by their base names (that is,
-- the part of the name up to the trailing .NNN).
groupByBaseName :: [TypePtr] -> IO (HashMap String [TypePtr])
groupByBaseName = foldM addToMap mempty
  where
    addToMap m tp = do
      -- All struct types that make it here are named.
      Just name <- cTypeName tp
      return $ HM.insertWith (++) (structBaseName name) [tp] m

-- | Unify a list of terms
unifies :: (Functor (e m), BindingMonad t v m, MonadTrans e, MonadError (UnificationFailure t v) (e m))
           => [UTerm t v] -> e m ()
unifies [] = return ()
unifies [_] = return ()
unifies (y:ys) = go ys y
  where
    go [] _ = return ()
    go (x:xs) z = unify x z >>= go xs

-- Helpers

-- | Like zip, but only returns a result if the two input lists are
-- the same length
pair :: [a] -> [b] -> Maybe [(a, b)]
pair xs ys =
  case length xs == length ys of
    False -> Nothing
    True -> Just (zip xs ys)

isStruct :: TypePtr -> IO Bool
isStruct tp = do
  tag <- cTypeTag tp
  case tag of
    TYPE_STRUCT -> do
      n <- cTypeName tp
      case n of
        Nothing -> return False
        Just _ -> return True
    _ -> return False

isNotOpaque :: TypePtr -> IO Bool
isNotOpaque tp = do
  ptrs <- cTypeList tp
  return (not (null ptrs))
