{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, RankNTypes, OverloadedStrings #-}
module LLVM.Internal.Interop where

import Control.Applicative
import Control.Monad ( when )
import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Array.Storable ( getElems, unsafeForeignPtrToStorableArray )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Data.LLVM.Types

#include "c++/marshal.h"

{#enum TypeTag {} deriving (Show, Eq) #}
{#enum ValueTag {underscoreToCase} deriving (Show, Eq) #}
{#enum MetaTag {underscoreToCase} deriving (Show, Eq) #}

data CModule
{#pointer *CModule as ModulePtr -> CModule #}

cModuleIdentifier :: ModulePtr -> IO ByteString
cModuleIdentifier m = ({#get CModule->moduleIdentifier#} m) >>= BS.packCString

cModuleDataLayout :: ModulePtr -> IO ByteString
cModuleDataLayout m = ({#get CModule->moduleDataLayout#} m) >>= BS.packCString

cModuleTargetTriple :: ModulePtr -> IO ByteString
cModuleTargetTriple m = ({#get CModule->targetTriple#} m) >>= BS.packCString

cModuleInlineAsm :: ModulePtr -> IO ByteString
cModuleInlineAsm m = ({#get CModule->moduleInlineAsm#} m) >>= BS.packCString

cModuleHasError :: ModulePtr -> IO Bool
cModuleHasError m = toBool <$> ({#get CModule->hasError#} m)

cModuleErrorMessage :: ModulePtr -> IO (Maybe String)
cModuleErrorMessage m = do
  hasError <- cModuleHasError m
  case hasError of
    True -> do
      msgPtr <- ({#get CModule->errMsg#} m)
      s <- peekCString msgPtr
      return (Just s)
    False -> return Nothing

cModuleLittleEndian :: ModulePtr -> IO Bool
cModuleLittleEndian m = toBool <$> ({#get CModule->littleEndian#} m)

cModulePointerSize :: ModulePtr -> IO Int
cModulePointerSize m = fromIntegral <$> ({#get CModule->pointerSize#} m)

cModuleGlobalVariables :: ModulePtr -> IO [ValuePtr]
cModuleGlobalVariables m =
  peekArray m {#get CModule->globalVariables#} {#get CModule->numGlobalVariables#}

cModuleGlobalAliases :: ModulePtr -> IO [ValuePtr]
cModuleGlobalAliases m =
  peekArray m ({#get CModule->globalAliases#}) ({#get CModule->numGlobalAliases#})

cModuleFunctions :: ModulePtr -> IO [ValuePtr]
cModuleFunctions m =
  peekArray m ({#get CModule->functions#}) ({#get CModule->numFunctions#})

peekArray :: forall a b c e . (Integral c, Storable e) =>
             a -> (a -> IO (Ptr b)) -> (a -> IO c) -> IO [e]
peekArray obj arrAccessor sizeAccessor = do
  nElts <- sizeAccessor obj
  arrPtr <- arrAccessor obj
  case nElts == 0 || arrPtr == nullPtr of
    True -> return []
    False -> do
      fArrPtr <- newForeignPtr_ (castPtr arrPtr)
      let elementCount :: Int
          elementCount = fromIntegral nElts
      arr <- unsafeForeignPtrToStorableArray fArrPtr (1, elementCount)
      getElems arr

data CType
{#pointer *CType as TypePtr -> CType #}
cTypeTag :: TypePtr -> IO TypeTag
cTypeTag t = toEnum . fromIntegral <$> {#get CType->typeTag#} t
cTypeSize :: TypePtr -> IO Int
cTypeSize t = fromIntegral <$> {#get CType->size#} t
cTypeIsVarArg :: TypePtr -> IO Bool
cTypeIsVarArg t = toBool <$> {#get CType->isVarArg#} t
cTypeIsPacked :: TypePtr -> IO Bool
cTypeIsPacked t = toBool <$> {#get CType->isPacked#} t
cTypeList :: TypePtr -> IO [TypePtr]
cTypeList t =
  peekArray t {#get CType->typeList#} {#get CType->typeListLen#}
cTypeInner :: TypePtr -> IO TypePtr
cTypeInner = {#get CType->innerType#}
cTypeName :: TypePtr -> IO (Maybe String)
cTypeName t = do
  n <- optionalField {#get CType->name#} t
  case n of
    Nothing -> return Nothing
    Just n' -> do
      s <- peekCString n'
      return (Just s)
cTypeAddrSpace :: TypePtr -> IO Int
cTypeAddrSpace t = fromIntegral <$> {#get CType->addrSpace#} t

data CValue
{#pointer *CValue as ValuePtr -> CValue #}
data CMeta
{#pointer *CMeta as MetaPtr -> CMeta #}

cValueTag :: ValuePtr -> IO ValueTag
cValueTag v = toEnum . fromIntegral <$> ({#get CValue->valueTag#} v)
cValueType :: ValuePtr -> IO TypePtr
cValueType = {#get CValue->valueType#}
cValueName :: ValuePtr -> IO (Maybe Identifier)
cValueName v = do
  tag <- cValueTag v
  namePtr <- ({#get CValue->name#}) v
  case namePtr == nullPtr of
    True -> return Nothing
    False -> do
      name <- BS.packCString namePtr
      case tag of
        ValFunction -> return $! (Just . makeGlobalIdentifier) name
        ValGlobalvariable -> return $! (Just . makeGlobalIdentifier) name
        ValAlias -> return $! (Just . makeGlobalIdentifier) name
        _ -> return $! (Just . makeLocalIdentifier) name
cValueMetadata :: ValuePtr -> IO [MetaPtr]
cValueMetadata v = peekArray v {#get CValue->md#} {#get CValue->numMetadata#}
cValueSrcLoc :: ValuePtr -> IO MetaPtr
cValueSrcLoc = {#get CValue->srcLoc#}
cValueData :: ValuePtr -> IO (Ptr ())
cValueData = {#get CValue->data#}


cMetaTypeTag :: MetaPtr -> IO MetaTag
cMetaTypeTag v = toEnum . fromIntegral <$> {#get CMeta->metaTag #} v

cMetaTag :: MetaPtr -> IO DW_TAG
cMetaTag p = dw_tag <$> {#get CMeta->tag#} p

cMetaArrayElts :: MetaPtr -> IO [Maybe MetaPtr]
cMetaArrayElts p = map convertNull <$>
  peekArray p {#get CMeta->u.metaArrayInfo.arrayElts#} {#get CMeta->u.metaArrayInfo.arrayLen#}
  where
    convertNull ptr =
      case ptr == nullPtr of
        True -> Nothing
        False -> Just ptr

cMetaEnumeratorName :: InternString m => MetaPtr -> m ByteString
cMetaEnumeratorName = shareString {#get CMeta->u.metaEnumeratorInfo.enumName#}
cMetaEnumeratorValue :: MetaPtr -> IO Int64
cMetaEnumeratorValue p = fromIntegral <$> {#get CMeta->u.metaEnumeratorInfo.enumValue#} p
cMetaGlobalContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaGlobalContext = optionalField {#get CMeta->u.metaGlobalInfo.context #}
cMetaGlobalName :: InternString m => MetaPtr -> m ByteString
cMetaGlobalName = shareString {#get CMeta->u.metaGlobalInfo.name#}
cMetaGlobalDisplayName :: InternString m => MetaPtr -> m ByteString
cMetaGlobalDisplayName = shareString {#get CMeta->u.metaGlobalInfo.displayName#}
cMetaGlobalLinkageName :: InternString m => MetaPtr -> m ByteString
cMetaGlobalLinkageName = shareString {#get CMeta->u.metaGlobalInfo.linkageName#}
-- cMetaGlobalCompileUnit :: MetaPtr -> IO MetaPtr
-- cMetaGlobalCompileUnit = {#get CMeta->u.metaGlobalInfo.compileUnit#}
cMetaGlobalLine :: MetaPtr -> IO Int32
cMetaGlobalLine p = fromIntegral <$> {#get CMeta->u.metaGlobalInfo.lineNumber#} p
cMetaGlobalType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaGlobalType = optionalField {#get CMeta->u.metaGlobalInfo.globalType#}
cMetaGlobalIsLocal :: MetaPtr -> IO Bool
cMetaGlobalIsLocal p = toBool <$> {#get CMeta->u.metaGlobalInfo.isLocalToUnit#} p
cMetaGlobalIsDefinition :: MetaPtr -> IO Bool
cMetaGlobalIsDefinition p = toBool <$> {#get CMeta->u.metaGlobalInfo.isDefinition#} p
cMetaLocationLine :: MetaPtr -> IO Int32
cMetaLocationLine p = fromIntegral <$> {#get CMeta->u.metaLocationInfo.lineNumber#} p
cMetaLocationColumn :: MetaPtr -> IO Int32
cMetaLocationColumn p = fromIntegral <$> {#get CMeta->u.metaLocationInfo.columnNumber#} p
cMetaLocationScope :: MetaPtr -> IO (Maybe MetaPtr)
cMetaLocationScope = optionalField {#get CMeta->u.metaLocationInfo.scope#}
cMetaSubrangeLo :: MetaPtr -> IO Int64
cMetaSubrangeLo p = fromIntegral <$> {#get CMeta->u.metaSubrangeInfo.lo#} p
cMetaSubrangeHi :: MetaPtr -> IO Int64
cMetaSubrangeHi p = fromIntegral <$> {#get CMeta->u.metaSubrangeInfo.hi#} p
cMetaTemplateTypeContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTemplateTypeContext = optionalField {#get CMeta->u.metaTemplateTypeInfo.context#}
cMetaTemplateTypeName :: InternString m => MetaPtr -> m ByteString
cMetaTemplateTypeName = shareString {#get CMeta->u.metaTemplateTypeInfo.name#}
cMetaTemplateTypeType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTemplateTypeType = optionalField {#get CMeta->u.metaTemplateTypeInfo.type#}
cMetaTemplateTypeLine :: MetaPtr -> IO Int32
cMetaTemplateTypeLine p = fromIntegral <$> {#get CMeta->u.metaTemplateTypeInfo.lineNumber#} p
cMetaTemplateTypeColumn :: MetaPtr -> IO Int32
cMetaTemplateTypeColumn p = fromIntegral <$> {#get CMeta->u.metaTemplateTypeInfo.columnNumber#} p
cMetaTemplateValueContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTemplateValueContext = optionalField {#get CMeta->u.metaTemplateValueInfo.context#}
cMetaTemplateValueName :: InternString m => MetaPtr -> m ByteString
cMetaTemplateValueName = shareString {#get CMeta->u.metaTemplateValueInfo.name#}
cMetaTemplateValueType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTemplateValueType = optionalField {#get CMeta->u.metaTemplateValueInfo.type#}
cMetaTemplateValueValue :: MetaPtr -> IO Int64
cMetaTemplateValueValue p = fromIntegral <$> {#get CMeta->u.metaTemplateValueInfo.value#} p
cMetaTemplateValueLine :: MetaPtr -> IO Int32
cMetaTemplateValueLine p = fromIntegral <$> {#get CMeta->u.metaTemplateValueInfo.lineNumber#} p
cMetaTemplateValueColumn :: MetaPtr -> IO Int32
cMetaTemplateValueColumn p = fromIntegral <$> {#get CMeta->u.metaTemplateValueInfo.columnNumber#} p
cMetaVariableContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaVariableContext = optionalField {#get CMeta->u.metaVariableInfo.context#}
cMetaVariableName :: InternString m => MetaPtr -> m ByteString
cMetaVariableName = shareString {#get CMeta->u.metaVariableInfo.name#}
-- cMetaVariableCompileUnit :: MetaPtr -> IO MetaPtr
-- cMetaVariableCompileUnit = {#get CMeta->u.metaVariableInfo.compileUnit#}
cMetaVariableLine :: MetaPtr -> IO Int32
cMetaVariableLine p = fromIntegral <$> {#get CMeta->u.metaVariableInfo.lineNumber#} p
cMetaVariableArgNumber :: MetaPtr -> IO Int32
cMetaVariableArgNumber p = fromIntegral <$> {#get CMeta->u.metaVariableInfo.argNumber#} p
cMetaVariableType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaVariableType = optionalField {#get CMeta->u.metaVariableInfo.type#}
cMetaVariableIsArtificial :: MetaPtr -> IO Bool
cMetaVariableIsArtificial p = toBool <$> {#get CMeta->u.metaVariableInfo.isArtificial#} p
cMetaVariableHasComplexAddress :: MetaPtr -> IO Bool
cMetaVariableHasComplexAddress p = toBool <$> {#get CMeta->u.metaVariableInfo.hasComplexAddress #} p
cMetaVariableAddrElements :: MetaPtr -> IO [Int64]
cMetaVariableAddrElements p = do
  ca <- cMetaVariableHasComplexAddress p
  case ca of
    True -> peekArray p {#get CMeta->u.metaVariableInfo.addrElements#} {#get CMeta->u.metaVariableInfo.numAddrElements#}
    False -> return []
cMetaVariableIsBlockByRefVar :: MetaPtr -> IO Bool
cMetaVariableIsBlockByRefVar p = toBool <$> {#get CMeta->u.metaVariableInfo.isBlockByRefVar#} p
cMetaCompileUnitLanguage :: MetaPtr -> IO DW_LANG
cMetaCompileUnitLanguage p = dw_lang <$> {#get CMeta->u.metaCompileUnitInfo.language#} p
cMetaCompileUnitFilename :: InternString m => MetaPtr -> m ByteString
cMetaCompileUnitFilename = shareString {#get CMeta->u.metaCompileUnitInfo.filename#}
cMetaCompileUnitDirectory :: InternString m => MetaPtr -> m ByteString
cMetaCompileUnitDirectory = shareString {#get CMeta->u.metaCompileUnitInfo.directory#}
cMetaCompileUnitProducer :: InternString m => MetaPtr -> m ByteString
cMetaCompileUnitProducer = shareString {#get CMeta->u.metaCompileUnitInfo.producer#}
cMetaCompileUnitIsMain :: MetaPtr -> IO Bool
cMetaCompileUnitIsMain p = toBool <$> {#get CMeta->u.metaCompileUnitInfo.isMain#} p
cMetaCompileUnitIsOptimized :: MetaPtr -> IO Bool
cMetaCompileUnitIsOptimized p = toBool <$> {#get CMeta->u.metaCompileUnitInfo.isOptimized#} p
cMetaCompileUnitFlags :: InternString m => MetaPtr -> m ByteString
cMetaCompileUnitFlags = shareString {#get CMeta->u.metaCompileUnitInfo.flags#}
cMetaCompileUnitRuntimeVersion :: MetaPtr -> IO Int32
cMetaCompileUnitRuntimeVersion p = fromIntegral <$> {#get CMeta->u.metaCompileUnitInfo.runtimeVersion#} p
cMetaCompileUnitEnumTypes :: MetaPtr -> IO (Maybe MetaPtr)
cMetaCompileUnitEnumTypes = optionalField {#get CMeta->u.metaCompileUnitInfo.enumTypes#}
cMetaCompileUnitRetainedTypes :: MetaPtr -> IO (Maybe MetaPtr)
cMetaCompileUnitRetainedTypes = optionalField {#get CMeta->u.metaCompileUnitInfo.retainedTypes#}
cMetaCompileUnitSubprograms :: MetaPtr -> IO (Maybe MetaPtr)
cMetaCompileUnitSubprograms = optionalField {#get CMeta->u.metaCompileUnitInfo.subprograms#}
cMetaCompileUnitGlobalVariables :: MetaPtr -> IO (Maybe MetaPtr)
cMetaCompileUnitGlobalVariables = optionalField {#get CMeta->u.metaCompileUnitInfo.globalVariables#}
cMetaFileFilename :: InternString m => MetaPtr -> m ByteString
cMetaFileFilename = shareString {#get CMeta->u.metaFileInfo.filename#}
cMetaFileDirectory :: InternString m => MetaPtr -> m ByteString
cMetaFileDirectory = shareString {#get CMeta->u.metaFileInfo.directory#}
-- cMetaFileCompileUnit :: MetaPtr -> IO MetaPtr
-- cMetaFileCompileUnit = {#get CMeta->u.metaFileInfo.compileUnit#}
cMetaLexicalBlockContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaLexicalBlockContext = optionalField {#get CMeta->u.metaLexicalBlockInfo.context#}
cMetaLexicalBlockLine :: MetaPtr -> IO Int32
cMetaLexicalBlockLine p = fromIntegral <$> {#get CMeta->u.metaLexicalBlockInfo.lineNumber#} p
cMetaLexicalBlockColumn :: MetaPtr -> IO Int32
cMetaLexicalBlockColumn p = fromIntegral <$> {#get CMeta->u.metaLexicalBlockInfo.columnNumber#} p
cMetaNamespaceContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaNamespaceContext = optionalField {#get CMeta->u.metaNamespaceInfo.context#}
cMetaNamespaceName :: InternString m => MetaPtr -> m ByteString
cMetaNamespaceName = shareString {#get CMeta->u.metaNamespaceInfo.name#}
-- cMetaNamespaceCompileUnit :: MetaPtr -> IO MetaPtr
-- cMetaNamespaceCompileUnit = {#get CMeta->u.metaNamespaceInfo.compileUnit#}
cMetaNamespaceLine :: MetaPtr -> IO Int32
cMetaNamespaceLine p = fromIntegral <$> {#get CMeta->u.metaNamespaceInfo.lineNumber#} p
cMetaSubprogramContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaSubprogramContext = optionalField {#get CMeta->u.metaSubprogramInfo.context#}
cMetaSubprogramName :: InternString m => MetaPtr -> m ByteString
cMetaSubprogramName = shareString {#get CMeta->u.metaSubprogramInfo.name#}
cMetaSubprogramDisplayName :: InternString m => MetaPtr -> m ByteString
cMetaSubprogramDisplayName = shareString {#get CMeta->u.metaSubprogramInfo.displayName#}
cMetaSubprogramLinkageName :: InternString m => MetaPtr -> m ByteString
cMetaSubprogramLinkageName = shareString {#get CMeta->u.metaSubprogramInfo.linkageName#}
-- cMetaSubprogramCompileUnit :: MetaPtr -> IO MetaPtr
-- cMetaSubprogramCompileUnit = {#get CMeta->u.metaSubprogramInfo.compileUnit#}
cMetaSubprogramLine :: MetaPtr -> IO Int32
cMetaSubprogramLine p = fromIntegral <$> {#get CMeta->u.metaSubprogramInfo.lineNumber#} p
cMetaSubprogramType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaSubprogramType = optionalField {#get CMeta->u.metaSubprogramInfo.type#}
cMetaSubprogramIsLocal :: MetaPtr -> IO Bool
cMetaSubprogramIsLocal p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isLocalToUnit#} p
cMetaSubprogramIsDefinition :: MetaPtr -> IO Bool
cMetaSubprogramIsDefinition p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isDefinition#} p
cMetaSubprogramVirtuality :: MetaPtr -> IO DW_VIRTUALITY
cMetaSubprogramVirtuality p = dw_virtuality <$> {#get CMeta->u.metaSubprogramInfo.virtuality#} p
cMetaSubprogramVirtualIndex :: MetaPtr -> IO Int32
cMetaSubprogramVirtualIndex p = fromIntegral <$> {#get CMeta->u.metaSubprogramInfo.virtualIndex#} p
cMetaSubprogramContainingType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaSubprogramContainingType = optionalField {#get CMeta->u.metaSubprogramInfo.containingType#}
cMetaSubprogramIsArtificial :: MetaPtr -> IO Bool
cMetaSubprogramIsArtificial p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isArtificial#} p
cMetaSubprogramIsPrivate :: MetaPtr -> IO Bool
cMetaSubprogramIsPrivate p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isPrivate#} p
cMetaSubprogramIsProtected :: MetaPtr -> IO Bool
cMetaSubprogramIsProtected p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isProtected#} p
cMetaSubprogramIsExplicit :: MetaPtr -> IO Bool
cMetaSubprogramIsExplicit p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isExplicit#} p
cMetaSubprogramIsPrototyped :: MetaPtr -> IO Bool
cMetaSubprogramIsPrototyped p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isPrototyped#} p
cMetaSubprogramIsOptimized :: MetaPtr -> IO Bool
cMetaSubprogramIsOptimized p = toBool <$> {#get CMeta->u.metaSubprogramInfo.isOptimized#} p
cMetaTypeContext :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeContext = optionalField {#get CMeta->u.metaTypeInfo.context#}
cMetaTypeName :: InternString m => MetaPtr -> m ByteString
cMetaTypeName = shareString {#get CMeta->u.metaTypeInfo.name#}
-- cMetaTypeCompileUnit :: MetaPtr -> IO (Maybe MetaPtr)
-- cMetaTypeCompileUnit = optionalField {#get CMeta->u.metaTypeInfo.compileUnit#}
cMetaTypeFile :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeFile = optionalField {#get CMeta->u.metaTypeInfo.file#}
cMetaTypeLine :: MetaPtr -> IO Int32
cMetaTypeLine p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.lineNumber#} p
cMetaTypeSize :: MetaPtr -> IO Int64
cMetaTypeSize p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.sizeInBits#} p
cMetaTypeAlign :: MetaPtr -> IO Int64
cMetaTypeAlign p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.alignInBits#} p
cMetaTypeOffset :: MetaPtr -> IO Int64
cMetaTypeOffset p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.offsetInBits#} p
cMetaTypeFlags :: MetaPtr -> IO Int32
cMetaTypeFlags p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.flags#} p
cMetaTypeIsPrivate :: MetaPtr -> IO Bool
cMetaTypeIsPrivate p = toBool <$> {#get CMeta->u.metaTypeInfo.isPrivate#} p
cMetaTypeIsProtected :: MetaPtr -> IO Bool
cMetaTypeIsProtected p = toBool <$> {#get CMeta->u.metaTypeInfo.isProtected#} p
cMetaTypeIsForward :: MetaPtr -> IO Bool
cMetaTypeIsForward p = toBool <$> {#get CMeta->u.metaTypeInfo.isForward#} p
cMetaTypeIsByRefStruct :: MetaPtr -> IO Bool
cMetaTypeIsByRefStruct p = toBool <$> {#get CMeta->u.metaTypeInfo.isByRefStruct#} p
cMetaTypeIsVirtual :: MetaPtr -> IO Bool
cMetaTypeIsVirtual p = toBool <$> {#get CMeta->u.metaTypeInfo.isVirtual#} p
cMetaTypeIsArtificial :: MetaPtr -> IO Bool
cMetaTypeIsArtificial p = toBool <$> {#get CMeta->u.metaTypeInfo.isArtificial#} p
cMetaTypeEncoding :: MetaPtr -> IO DW_ATE
cMetaTypeEncoding p = dw_ate <$> {#get CMeta->u.metaTypeInfo.encoding#} p
cMetaTypeDerivedFrom :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeDerivedFrom = optionalField {#get CMeta->u.metaTypeInfo.typeDerivedFrom#}
cMetaTypeCompositeComponents :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeCompositeComponents = optionalField {#get CMeta->u.metaTypeInfo.typeArray#}
cMetaTypeRuntimeLanguage :: MetaPtr -> IO Int32
cMetaTypeRuntimeLanguage p = fromIntegral <$> {#get CMeta->u.metaTypeInfo.runTimeLang#} p
cMetaTypeContainingType :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeContainingType = optionalField {#get CMeta->u.metaTypeInfo.containingType#}
cMetaTypeTemplateParams :: MetaPtr -> IO (Maybe MetaPtr)
cMetaTypeTemplateParams = optionalField {#get CMeta->u.metaTypeInfo.templateParams#}
cMetaUnknownRepr :: InternString m => MetaPtr -> m ByteString
cMetaUnknownRepr = shareString {#get CMeta->u.metaUnknownInfo.repr#}

optionalField :: (a -> IO (Ptr b)) -> a -> IO (Maybe (Ptr b))
optionalField accessor p = do
  v <- accessor p
  case v == nullPtr of
    True -> return Nothing
    False -> return (Just v)

class MonadIO m => InternString m where
  internString :: ByteString -> m ByteString

-- | This helper converts C char* strings into ByteStrings, sharing
-- identical bytestrings on the Haskell side.  This is a simple
-- space-saving optimization (assuming the entire cache is garbage
-- collected)
shareString :: InternString m => (a -> IO CString) -> a -> m ByteString
shareString accessor ptr = do
  sp <- liftIO $ accessor ptr
  when (sp == nullPtr) (error "Null ptr in string")
  str <- liftIO $ BS.packCString sp
  internString str
  {-
  s <- get
  let cache = stringCache s
  case M.lookup str cache of
    Just cval -> return cval
    Nothing -> do
      put s { stringCache = M.insert str str cache }
      return str
-}
data CGlobalInfo
{#pointer *CGlobalInfo as GlobalInfoPtr -> CGlobalInfo #}
cGlobalIsExternal :: GlobalInfoPtr -> IO Bool
cGlobalIsExternal g = toBool <$> ({#get CGlobalInfo->isExternal#} g)
cGlobalAlignment :: GlobalInfoPtr -> IO Int64
cGlobalAlignment g = fromIntegral <$> ({#get CGlobalInfo->alignment#} g)
cGlobalVisibility :: GlobalInfoPtr -> IO VisibilityStyle
cGlobalVisibility g = toEnum . fromIntegral <$> ({#get CGlobalInfo->visibility#} g)
cGlobalLinkage :: GlobalInfoPtr -> IO LinkageType
cGlobalLinkage g = toEnum . fromIntegral <$> ({#get CGlobalInfo->linkage#} g)
cGlobalSection :: GlobalInfoPtr -> IO (Maybe ByteString)
cGlobalSection g = do
  s <- {#get CGlobalInfo->section#} g
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
cGlobalInitializer :: GlobalInfoPtr -> IO ValuePtr
cGlobalInitializer = {#get CGlobalInfo->initializer#}
cGlobalIsThreadLocal :: GlobalInfoPtr -> IO Bool
cGlobalIsThreadLocal g = toBool <$> ({#get CGlobalInfo->isThreadLocal#} g)
cGlobalAliasee :: GlobalInfoPtr -> IO ValuePtr
cGlobalAliasee = {#get CGlobalInfo->aliasee#}
cGlobalIsConstant :: GlobalInfoPtr -> IO Bool
cGlobalIsConstant g = toBool <$> {#get CGlobalInfo->isConstant#} g

data CFunctionInfo
{#pointer *CFunctionInfo as FunctionInfoPtr -> CFunctionInfo #}
cFunctionIsExternal :: FunctionInfoPtr -> IO Bool
cFunctionIsExternal f = toBool <$> {#get CFunctionInfo->isExternal#} f
cFunctionAlignment :: FunctionInfoPtr -> IO Int64
cFunctionAlignment f = fromIntegral <$> {#get CFunctionInfo->alignment#} f
cFunctionVisibility :: FunctionInfoPtr -> IO VisibilityStyle
cFunctionVisibility f = toEnum . fromIntegral <$> {#get CFunctionInfo->visibility#} f
cFunctionLinkage :: FunctionInfoPtr -> IO LinkageType
cFunctionLinkage f = toEnum . fromIntegral <$> {#get CFunctionInfo->linkage#} f
cFunctionSection :: FunctionInfoPtr -> IO (Maybe ByteString)
cFunctionSection f = do
  s <- {#get CFunctionInfo->section#} f
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
cFunctionIsVarArg :: FunctionInfoPtr -> IO Bool
cFunctionIsVarArg f = toBool <$> {#get CFunctionInfo->isVarArg#} f
cFunctionCallingConvention :: FunctionInfoPtr -> IO CallingConvention
cFunctionCallingConvention f = toEnum . fromIntegral <$> {#get CFunctionInfo->callingConvention#} f
cFunctionGCName :: FunctionInfoPtr -> IO (Maybe ByteString)
cFunctionGCName f = do
  s <- {#get CFunctionInfo->gcName#} f
  case s == nullPtr of
    True -> return Nothing
    False -> do
      bs <- BS.packCString s
      return $! Just bs
cFunctionArguments :: FunctionInfoPtr -> IO [ValuePtr]
cFunctionArguments f =
  peekArray f {#get CFunctionInfo->arguments#} {#get CFunctionInfo->argListLen#}
cFunctionBlocks :: FunctionInfoPtr -> IO [ValuePtr]
cFunctionBlocks f =
  peekArray f {#get CFunctionInfo->body#} {#get CFunctionInfo->blockListLen#}

data CArgInfo
{#pointer *CArgumentInfo as ArgInfoPtr -> CArgInfo #}
cArgInfoHasSRet :: ArgInfoPtr -> IO Bool
cArgInfoHasSRet a = toBool <$> ({#get CArgumentInfo->hasSRet#} a)
cArgInfoHasByVal :: ArgInfoPtr -> IO Bool
cArgInfoHasByVal a = toBool <$> ({#get CArgumentInfo->hasByVal#} a)
cArgInfoHasNest :: ArgInfoPtr -> IO Bool
cArgInfoHasNest a = toBool <$> ({#get CArgumentInfo->hasNest#} a)
cArgInfoHasNoAlias :: ArgInfoPtr -> IO Bool
cArgInfoHasNoAlias a = toBool <$> ({#get CArgumentInfo->hasNoAlias#} a)
cArgInfoHasNoCapture :: ArgInfoPtr -> IO Bool
cArgInfoHasNoCapture a = toBool <$> ({#get CArgumentInfo->hasNoCapture#} a)

data CBasicBlockInfo
{#pointer *CBasicBlockInfo as BasicBlockPtr -> CBasicBlockInfo #}

cBasicBlockInstructions :: BasicBlockPtr -> IO [ValuePtr]
cBasicBlockInstructions b =
  peekArray b {#get CBasicBlockInfo->instructions#} {#get CBasicBlockInfo->blockLen#}

data CInlineAsmInfo
{#pointer *CInlineAsmInfo as InlineAsmInfoPtr -> CInlineAsmInfo #}

cInlineAsmString :: InlineAsmInfoPtr -> IO ByteString
cInlineAsmString a =
  ({#get CInlineAsmInfo->asmString#} a) >>= BS.packCString
cInlineAsmConstraints :: InlineAsmInfoPtr -> IO ByteString
cInlineAsmConstraints a =
  ({#get CInlineAsmInfo->constraintString#} a) >>= BS.packCString

data CBlockAddrInfo
{#pointer *CBlockAddrInfo as BlockAddrInfoPtr -> CBlockAddrInfo #}

cBlockAddrFunc :: BlockAddrInfoPtr -> IO ValuePtr
cBlockAddrFunc = {#get CBlockAddrInfo->func #}
cBlockAddrBlock :: BlockAddrInfoPtr -> IO ValuePtr
cBlockAddrBlock = {#get CBlockAddrInfo->block #}

data CAggregateInfo
{#pointer *CConstAggregate as AggregateInfoPtr -> CAggregateInfo #}

cAggregateValues :: AggregateInfoPtr -> IO [ValuePtr]
cAggregateValues a =
  peekArray a {#get CConstAggregate->constants#} {#get CConstAggregate->numElements#}

data CConstFP
{#pointer *CConstFP as FPInfoPtr -> CConstFP #}
cFPVal :: FPInfoPtr -> IO Double
cFPVal f = realToFrac <$> ({#get CConstFP->val#} f)

data CConstInt
{#pointer *CConstInt as IntInfoPtr -> CConstInt #}
cIntVal :: IntInfoPtr -> IO Integer
cIntVal i = fromIntegral <$> ({#get CConstInt->val#} i)

data CInstructionInfo
{#pointer *CInstructionInfo as InstInfoPtr -> CInstructionInfo #}
cInstructionOperands :: InstInfoPtr -> IO [ValuePtr]
cInstructionOperands i =
  peekArray i {#get CInstructionInfo->operands#} {#get CInstructionInfo->numOperands#}
cInstructionArithFlags :: InstInfoPtr -> IO ArithFlags
cInstructionArithFlags o = toEnum . fromIntegral <$> {#get CInstructionInfo->flags#} o
cInstructionAlign :: InstInfoPtr -> IO Int64
cInstructionAlign u = fromIntegral <$> {#get CInstructionInfo->align#} u
cInstructionIsVolatile :: InstInfoPtr -> IO Bool
cInstructionIsVolatile u = toBool <$> {#get CInstructionInfo->isVolatile#} u
cInstructionAddrSpace :: InstInfoPtr -> IO Int
cInstructionAddrSpace u = fromIntegral <$> {#get CInstructionInfo->addrSpace#} u
cInstructionCmpPred :: InstInfoPtr -> IO CmpPredicate
cInstructionCmpPred c = toEnum . fromIntegral <$> {#get CInstructionInfo->cmpPred#} c
cInstructionInBounds :: InstInfoPtr -> IO Bool
cInstructionInBounds g = toBool <$> {#get CInstructionInfo->inBounds#} g
cInstructionIndices :: InstInfoPtr -> IO [Int]
cInstructionIndices i =
  peekArray i {#get CInstructionInfo->indices#} {#get CInstructionInfo->numIndices#}

data CConstExprInfo
{#pointer *CConstExprInfo as ConstExprPtr -> CConstExprInfo #}
cConstExprTag :: ConstExprPtr -> IO ValueTag
cConstExprTag e = toEnum . fromIntegral <$> {#get CConstExprInfo->instrType#} e
cConstExprInstInfo :: ConstExprPtr -> IO InstInfoPtr
cConstExprInstInfo = {#get CConstExprInfo->ii#}

data CPHIInfo
{#pointer *CPHIInfo as PHIInfoPtr -> CPHIInfo #}
cPHIValues :: PHIInfoPtr -> IO [ValuePtr]
cPHIValues p =
  peekArray p {#get CPHIInfo->incomingValues#} {#get CPHIInfo->numIncomingValues#}
cPHIBlocks :: PHIInfoPtr -> IO [ValuePtr]
cPHIBlocks p =
  peekArray p {#get CPHIInfo->valueBlocks#} {#get CPHIInfo->numIncomingValues#}

data CCallInfo
{#pointer *CCallInfo as CallInfoPtr -> CCallInfo #}
cCallValue :: CallInfoPtr -> IO ValuePtr
cCallValue = {#get CCallInfo->calledValue #}
cCallArguments :: CallInfoPtr -> IO [ValuePtr]
cCallArguments c =
  peekArray c {#get CCallInfo->arguments#} {#get CCallInfo->argListLen#}
cCallConvention :: CallInfoPtr -> IO CallingConvention
cCallConvention c = toEnum . fromIntegral <$> {#get CCallInfo->callingConvention#} c
cCallHasSRet :: CallInfoPtr -> IO Bool
cCallHasSRet c = toBool <$> {#get CCallInfo->hasSRet#} c
cCallIsTail :: CallInfoPtr -> IO Bool
cCallIsTail c = toBool <$> {#get CCallInfo->isTail#} c
cCallUnwindDest :: CallInfoPtr -> IO ValuePtr
cCallUnwindDest = {#get CCallInfo->unwindDest#}
cCallNormalDest :: CallInfoPtr -> IO ValuePtr
cCallNormalDest = {#get CCallInfo->normalDest#}

data CAtomicInfo
{#pointer *CAtomicInfo as AtomicInfoPtr -> CAtomicInfo #}
cAtomicOrdering :: AtomicInfoPtr -> IO AtomicOrdering
cAtomicOrdering ai = toEnum . fromIntegral <$> {#get CAtomicInfo->ordering#} ai
cAtomicScope :: AtomicInfoPtr -> IO SynchronizationScope
cAtomicScope ai = toEnum . fromIntegral <$> {#get CAtomicInfo->scope#} ai
cAtomicOperation :: AtomicInfoPtr -> IO AtomicOperation
cAtomicOperation ai = toEnum . fromIntegral <$> {#get CAtomicInfo->operation#} ai
cAtomicIsVolatile :: AtomicInfoPtr -> IO Bool
cAtomicIsVolatile ai = toBool <$> {#get CAtomicInfo->isVolatile#} ai
cAtomicAddressSpace :: AtomicInfoPtr -> IO Int
cAtomicAddressSpace ai = fromIntegral <$> {#get CAtomicInfo->addrSpace#} ai
cAtomicPointerOperand :: AtomicInfoPtr -> IO ValuePtr
cAtomicPointerOperand = {#get CAtomicInfo->pointerOperand#}
cAtomicValueOperand :: AtomicInfoPtr -> IO ValuePtr
cAtomicValueOperand = {#get CAtomicInfo->valueOperand#}
cAtomicCompareOperand :: AtomicInfoPtr -> IO ValuePtr
cAtomicCompareOperand = {#get CAtomicInfo->compareOperand#}

data CLandingPadInfo
{#pointer *CLandingPadInfo as LandingPadInfoPtr -> CLandingPadInfo#}
cLandingPadPersonality :: LandingPadInfoPtr -> IO ValuePtr
cLandingPadPersonality = {#get CLandingPadInfo->personality#}
cLandingPadIsCleanup :: LandingPadInfoPtr -> IO Bool
cLandingPadIsCleanup li = toBool <$> {#get CLandingPadInfo->isCleanup#} li
cLandingPadClauses :: LandingPadInfoPtr -> IO [ValuePtr]
cLandingPadClauses li =
  peekArray li {#get CLandingPadInfo->clauses #} {#get CLandingPadInfo->numClauses#}
cLandingPadClauseTypes :: LandingPadInfoPtr -> IO [LandingPadClause]
cLandingPadClauseTypes li = do
  arr <- peekArray li {#get CLandingPadInfo->clauseTypes #} {#get CLandingPadInfo->numClauses#}
  return $ map toEnum arr

-- | Parse the named file into an FFI-friendly representation of an
-- LLVM module.
{#fun marshalLLVM { id `Ptr CChar', `Int',  fromBool `Bool' } -> `ModulePtr' id #}
{#fun marshalLLVMFile { `String', fromBool `Bool' } -> `ModulePtr' id #}

-- | Free all of the resources allocated by 'marshalLLVM'
{#fun disposeCModule { id `ModulePtr' } -> `()' #}
