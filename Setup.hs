import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.PackageDescription

import Data.LLVM.Internal.Paths

main = defaultMainWithHooks autoconfUserHooks { preBuild = extBuild }

extBuild args flags = do
  -- Use the Paths module exposed by one of the dependencies to find
  -- the directory containing the enumeration header.  Add that
  -- directory to the search path for this package.
  enumHeaderDir <- getDataFileName "src/c++"
  (Just bi, rest) <- (preBuild autoconfUserHooks) args flags
  let incdirs = includeDirs bi
  return (Just bi { includeDirs = enumHeaderDir : incdirs }, rest)

