module Main where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import qualified IHP.Server
import IHP.RouterSupport
import IHP.ControllerPrelude
import IHP.Mail
--import IHP.GenericController

data DemoController = DemoAction deriving (Eq, Show, Data)

instance AutoRoute DemoController
instance InitControllerContext RootApplication
instance FrontController RootApplication where
    controllers =
        [ parseRoute @DemoController
        , startPage DemoAction
        ]

instance Controller DemoController where
    action DemoAction = renderPlain "Hello World!"

config :: IO FrameworkConfig
config = do
    appPort <- defaultAppPort
    databaseUrl <- defaultDatabaseUrl
    let
        environment = Development
        appHostname = "localhost"
        baseUrl = let port = appPort in "http://" <> appHostname <> (if port /= 80 then ":" <> tshow port else "")
        requestLoggerMiddleware = defaultLoggerMiddleware
        sessionCookie = defaultIHPSessionCookie baseUrl
        mailServer = Sendmail
        dbPoolIdleTime = 60
        dbPoolMaxConnections = 20

    pure FrameworkConfig {..}

main :: IO ()
main = config >>= IHP.Server.run
