module Main where

import           Data.Foldable
import           Data.List
import           Data.Monoid
import qualified Data.Set as S
import           Distribution.License
import           Distribution.ModuleName hiding (main)
import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Distribution.PackageDescription.PrettyPrint
import           Distribution.Types.PackageName
import           Distribution.Verbosity
import           Distribution.Version
import           Language.Haskell.Extension
import           System.Environment

data State =
  State {
    sExposedModules  :: [ ModuleName ]
  , sLibDepends      :: S.Set Dependency
  , sNamesToExclude  :: [ PackageName ]
  , sLibExtensions   :: S.Set Extension
  , sLibOtherModules :: S.Set ModuleName
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
  result <- getArgs >>= foldlM go (State [] S.empty [] S.empty S.empty)
  let
    genPackage = GenericPackageDescription {
        packageDescription = pkgDesc
      , condBenchmarks = []
      , condTestSuites = []
      , condExecutables = []
      , condSubLibraries = []
      , genPackageFlags = []
      , condLibrary = Just libNode
      , condForeignLibs = []
      }
    libNode = CondNode {
        condTreeComponents = []
      , condTreeData = mergedLib
      , condTreeConstraints = []
      }
    libFilter :: Dependency -> Bool
    libFilter dep = notElem (depPkgName dep) (sNamesToExclude result)
    filteredLibDepends = filter libFilter (S.toList $ sLibDepends result)
    pathsFilter :: ModuleName -> Bool
    pathsFilter mod = not (isPrefixOf "Paths_" (toFilePath mod))
    mergedLib = Library {
        exposedModules = filter pathsFilter $ sExposedModules result
      , libBuildInfo = emptyBuildInfo {
            buildable = True
          , defaultLanguage = Just Haskell2010
          , defaultExtensions = S.toList $ sLibExtensions result
          , otherModules = filter pathsFilter $ S.toList $ sLibOtherModules result
          , hsSourceDirs = [
              "wallet-new/src"
            , "utxo/src"
            , "x509/src"
            , "wallet/src"
            , "wallet/test"
            , "util/src"
            , "node-ipc/src"
            , "networking/src"
            , "db/src"
            , "infra/src"
            , "generator/src"
            , "crypto"
            , "client/src"
            , "core/src"
            , "binary/src"
            , "chain/src"
            , "acid-state-exts/src"
            , "lib/src"
            , "binary/test"
            , "chain/test"
            , "core/test"
            , "crypto/test"
            , "util/test"
            ]
          , targetBuildDepends = filteredLibDepends <> ([ Dependency "unix" AnyVersion, Dependency "systemd" AnyVersion ])
          }
      , reexportedModules = []
      , signatures = []
      , libExposed = True
      }
    pkgDesc = PackageDescription {
        package = PackageIdentifier {pkgName = mkPackageName "everything", pkgVersion = mkVersion [1,3,0]}
      , license = MIT
      , customFieldsPD = []
      , sourceRepos = []
      , specVersionRaw = Right (UnionVersionRanges (ThisVersion (mkVersion [1,10])) (LaterVersion (mkVersion [1,10])))
      , buildType = Just Simple
      , licenseFiles = []
      , copyright = ""
      , maintainer = "operations@iohk.io"
      , stability = ""
      , homepage = "https://github.com/input-output-hk/cardano-sl/#readme"
      , pkgUrl = ""
      , bugReports = ""
      , synopsis = ""
      , description = ""
      , category = ""
      , author = ""
      , testedWith = []
      , dataFiles = []
      , dataDir = ""
      , extraSrcFiles = []
      , extraTmpFiles = []
      , extraDocFiles = []
      }
  writeGenericPackageDescription "output.cabal" genPackage

stripVersionRestrictions :: GenericPackageDescription -> GenericPackageDescription
stripVersionRestrictions = id
