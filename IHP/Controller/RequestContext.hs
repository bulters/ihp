module IHP.Controller.RequestContext
( RequestContext (..)
, Respond
) where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LBS
import           Network.Wai                   (Request, Response, ResponseReceived)
import           Network.Wai.Parse (File, Param)
import qualified Data.Vault.Lazy               as Vault
import           Network.Wai.Session           (Session)
import           IHP.FrameworkConfig           (FrameworkConfig, FrameworkConfigProxy, extractConfig)

type Respond = Response -> IO ResponseReceived

data RequestContext = RequestContext
    { request :: Request
    , respond :: Respond
    , params :: [Param]
    , files :: [File LBS.ByteString]
    , vault :: (Vault.Key (Session IO String String))
    , frameworkConfig :: FrameworkConfig
    }

instance FrameworkConfigProxy RequestContext where
    extractConfig = frameworkConfig
