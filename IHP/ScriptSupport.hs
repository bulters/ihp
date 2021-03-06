{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script) where

import IHP.Prelude
import qualified IHP.FrameworkConfig as Config
import qualified IHP.Environment as Env
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple as PG

-- | A script is just an IO action which requires a database connection
type Script = (?modelContext :: ModelContext) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: Config.FrameworkConfig => Script -> IO ()
runScript taskMain = do
    databaseUrl <- Config.appDatabaseUrl
    modelContext <- (\modelContext -> modelContext { queryDebuggingEnabled = Env.isDevelopment Config.environment }) <$> createModelContext Config.dbPoolIdleTime Config.dbPoolMaxConnections databaseUrl

    let ?modelContext = modelContext
    taskMain
