module OwlCore.Repl.Repl where

import Globals
import qualified OwlCore.Syntax.AST as AST
import qualified OwlCore.Syntax.PPrint as PPrint
import qualified OwlCore.Parser.Grammar as Parser
  
import System.Console.Haskeline
import Control.Monad.State.Lazy

-- A record containing the Repl configuration.
data Config = Config {
  promptStr :: String,
  inputFilePath :: String
}

defaultPrompt :: String
defaultPrompt = "owlCore> "

defaultInputFilePath :: String
defaultInputFilePath = ""

initConfig :: Config
initConfig = Config defaultPrompt defaultInputFilePath

-- State monad that contains an instance of the configuration options.
type ReplState a = StateT Config IO a

-- ReplState helpers to make it easier to set options.
setFilePath :: String -> ReplState ()
setFilePath s = do
  opts <- get
  do case opts of
       -- Currently, we only have one configuration option, and it's
       -- the file path, so we just ignore it, and then set the new
       -- one.
       Config p _ -> put $ Config p s

getPromptStr :: ReplState String
getPromptStr = get >>= (return . promptStr)

-- The repl banner:
owlUnicode :: String
owlUnicode = "🦉"

banner :: String
banner = owlUnicode ++ " Welcome to OwlCore " ++ version ++ " " ++ owlUnicode

-- Repl main loop in the configuration state monad + IO.
repl :: ReplState ()
repl = do
  config <- get
  lift $ runInputT defaultSettings (loop config)
   where
       loop :: Config -> InputT IO ()
       loop config = do
           outputStrLn banner
           innerLoop config
           
       innerLoop :: Config -> InputT IO ()
       innerLoop config@(Config promptStr _) = do
           inputM <- getInputLine promptStr           
           case inputM of
               Nothing -> return ()
               Just ":q" -> return ()
               Just input -> do                 
                 outputStrLn . (PPrint.pprint) $ Parser.parseCore input
                 innerLoop config

-- The main function of the repl. `Main.main` simply calls this
-- function.
runRepl :: IO ()
runRepl = evalStateT repl initConfig
