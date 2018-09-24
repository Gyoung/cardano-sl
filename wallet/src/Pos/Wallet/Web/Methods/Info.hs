-- | Client information.

module Pos.Wallet.Web.Methods.Info
       ( getClientInfo
       ) where

import           Universum

import           Pos.Chain.Update (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo,
                     ctiGitRevision)
import           Pos.Wallet.Web.ClientTypes (ApiVersion (..), ClientInfo (..))
import Data.Version (Version(Version))

getClientInfo :: (HasCompileInfo, HasUpdateConfiguration, Applicative m) => m ClientInfo
getClientInfo =
    pure
        ClientInfo
        { ciGitRevision = ctiGitRevision compileInfo
        , ciSoftwareVersion = curSoftwareVersion
        , ciCabalVersion = Version [ 1, 3, 0 ] []
        , ciApiVersion = ApiVersion0
        }
