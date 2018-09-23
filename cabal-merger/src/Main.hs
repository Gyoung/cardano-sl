module Main where

import           Data.Either
import           Data.Foldable (foldlM)
import           Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Distribution.License (License (MIT))
import           Distribution.ModuleName (ModuleName, toFilePath)
import           Distribution.Package (Dependency (Dependency),
                     PackageIdentifier (PackageIdentifier, pkgName, pkgVersion),
                     PackageName, depPkgName, mkPackageName)
import           Distribution.PackageDescription (BuildInfo (buildable, defaultExtensions, defaultLanguage, hsSourceDirs, otherModules, targetBuildDepends),
                     BuildType (Simple),
                     CondTree (CondNode, condTreeComponents, condTreeConstraints, condTreeData),
                     ConfVar, Executable (buildInfo),
                     Library (exposedModules, libBuildInfo),
                     PackageDescription (buildTypeRaw, homepage, licenseRaw, maintainer, package, specVersionRaw),
                     emptyBuildInfo, emptyLibrary, emptyPackageDescription,
                     executables)
import           Distribution.PackageDescription.Parsec
                     (readGenericPackageDescription)
import           Distribution.PackageDescription.PrettyPrint
                     (writeGenericPackageDescription)
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (condExecutables, condLibrary, packageDescription),
                     emptyGenericPackageDescription)
import           Distribution.Types.UnqualComponentName (UnqualComponentName)
import           Distribution.Verbosity (silent)
import           Distribution.Version (anyVersion, laterVersion, mkVersion,
                     thisVersion, unionVersionRanges)
import           Filesystem.Path (directory)
import           Filesystem.Path.CurrentOS
import           Language.Haskell.Extension (Extension, Language (Haskell2010))
import           System.Environment (getArgs)

data State =
  State {
    sExposedModules  :: [ ModuleName ]
  , sLibDepends      :: S.Set Dependency
  , sNamesToExclude  :: [ PackageName ]
  , sLibExtensions   :: S.Set Extension
  , sLibOtherModules :: S.Set ModuleName
  , sExecutables     :: [ Executable ]
  , sCondExecutables :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
  } deriving Show

instance Ord Dependency where
  compare a b = compare (depPkgName a) (depPkgName b)

main :: IO ()
main = do
  let
    go :: State -> String -> IO State
    go state cabalFile = do
      pkg <- readGenericPackageDescription silent cabalFile
      print cabalFile
      let
        prefix = directory (fromText $ T.pack cabalFile)
      middle <- goLibrary state pkg
      final <- foldlM (goExecutable $ T.unpack $ fromRight undefined $ toText prefix) middle (condExecutables pkg)
      pure $ final {
          sExecutables = (sExecutables final) <> (executables $ packageDescription pkg)
        }

    fixExecutableSrc :: String -> Executable -> Executable
    fixExecutableSrc prefix exe = exe {
        buildInfo = (buildInfo exe) {
            hsSourceDirs = map (prefix <>) (hsSourceDirs $ buildInfo exe)
        }
      }

    goLibrary :: State -> GenericPackageDescription -> IO State
    goLibrary state pkg = do
      case (condLibrary pkg) of
        Just node -> do
          let
            pkgname :: PackageName
            pkgname = pkgName $ package $ packageDescription pkg
            eModules = exposedModules $ condTreeData node
            newState = state {
                sExposedModules = (sExposedModules state) <> eModules
              , sLibDepends = (sLibDepends state) <> (S.fromList $ targetBuildDepends $ libBuildInfo $ condTreeData node)
              , sNamesToExclude = (sNamesToExclude state) <> [ pkgname ]
              , sLibExtensions = (sLibExtensions state) <> (S.fromList $ defaultExtensions $ libBuildInfo $ condTreeData node)
              , sLibOtherModules = (sLibOtherModules state) <> (S.fromList $ otherModules $ libBuildInfo $ condTreeData node)
              }
          pure newState
        Nothing -> do
          pure state
    goExecutable :: String -> State -> (UnqualComponentName, CondTree ConfVar [ Dependency ] Executable) -> IO State
    goExecutable prefix state (name, CondNode exe _ _) = do
      -- by omiting half of the CondNode, the conditions within each executable section are missing
      pure $ state {
        sCondExecutables = (sCondExecutables state) <> [ (name, CondNode (fixExecutableSrc prefix exe) [] [] ) ]
      }
  result <- getArgs >>= foldlM go (State [] S.empty [] S.empty S.empty [] [])
  let
    genPackage = emptyGenericPackageDescription {
        packageDescription = pkgDesc
      , condLibrary = Just libNode
      , condExecutables = filterCondExecutables $ sCondExecutables result
      }
    libNode :: CondTree ConfVar [Dependency] Library
    libNode = CondNode {
        condTreeComponents = []
      , condTreeData = mergedLib
      , condTreeConstraints = []
      }
    libFilter :: Dependency -> Bool
    libFilter dep = notElem (depPkgName dep) (sNamesToExclude result)
    filteredLibDepends :: [Dependency]
    filteredLibDepends = filter libFilter (S.toList $ sLibDepends result)
    pathsFilter :: ModuleName -> Bool
    pathsFilter = not . isPrefixOf "Paths_" . toFilePath
    mergedLib = emptyLibrary {
        exposedModules = filter pathsFilter $ sExposedModules result
      , libBuildInfo = emptyBuildInfo {
            buildable = True
          , defaultLanguage = Just Haskell2010
          , defaultExtensions = S.toList $ sLibExtensions result
          , otherModules = filter pathsFilter $ S.toList $ sLibOtherModules result
          , hsSourceDirs = [
              "acid-state-exts/src"
            , "binary/src"
            , "binary/test"
            , "chain/src"
            , "chain/test"
            , "client/src"
            , "core/src"
            , "core/test"
            , "crypto"
            , "crypto/test"
            , "db/src"
            , "generator/src"
            , "infra/src"
            , "lib/src"
            , "networking/src"
            , "node-ipc/src"
            , "util/src"
            , "util/test"
            , "utxo/src"
            , "wallet-new/src"
            , "wallet/src"
            , "wallet/test"
            , "x509/src"
            ]
          , targetBuildDepends = filteredLibDepends <> ([ Dependency "unix" anyVersion, Dependency "systemd" anyVersion ])
          }
      }
    filterCondExecutables :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)] -> [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)]
    filterCondExecutables = map exeFilter2
    filterExecutables :: [ Executable ] -> [ Executable ]
    filterExecutables = map exeFilter
    exeFilter :: Executable -> Executable
    exeFilter exe = exe {
      buildInfo = (buildInfo exe) {
        targetBuildDepends = (filter libFilter (targetBuildDepends (buildInfo exe))) <> [ Dependency "everything" anyVersion ]
      }
    }
    exeFilter2 :: (UnqualComponentName, CondTree ConfVar [Dependency] Executable) -> (UnqualComponentName, CondTree ConfVar [Dependency] Executable)
    exeFilter2 (name, CondNode exe deps conf) = (name, CondNode (exeFilter exe) (filter libFilter deps) conf)
    pkgDesc = emptyPackageDescription {
        package = PackageIdentifier {pkgName = mkPackageName "everything", pkgVersion = mkVersion [1,3,0]}
      , licenseRaw = Right MIT
      , specVersionRaw = Right (unionVersionRanges (thisVersion (mkVersion [1,10])) (laterVersion (mkVersion [1,10])))
      , buildTypeRaw = Just Simple
      , executables = filterExecutables $ sExecutables result
      , maintainer = "operations@iohk.io"
      , homepage = "https://github.com/input-output-hk/cardano-sl/#readme"
      }
  writeGenericPackageDescription "output" genPackage
