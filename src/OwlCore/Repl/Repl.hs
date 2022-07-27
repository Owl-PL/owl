module OwlCore.Repl.Repl (runRepl) where

import Globals
import qualified OwlCore.Syntax.AST as AST
import qualified OwlCore.Syntax.PPrint as PPrint
import qualified OwlCore.Syntax.Parser as Parser
  
import System.Console.Haskeline
import Control.Monad.State.Lazy

-- | The Repl's configuration record.
data Config = Config {
  -- | The prompt, whose default is `defaultPrompt`.
  promptStr :: String,
  -- | The input file to load into the Repl state.
  inputFilePath :: String 
}

-- | The default prompt.
defaultPrompt :: String
defaultPrompt = "owlCore> "

-- | The default input file path; the empty string.
defaultInputFilePath :: String
defaultInputFilePath = ""

-- | The initial configuration.
initConfig :: Config
initConfig = Config defaultPrompt defaultInputFilePath

-- | The state monad for the repl housing the configuration and access
--   to IO.
type ReplState a = StateT Config IO a

-- | Sets the file path in the `ReplState`. 
setFilePath :: String -> ReplState ()
setFilePath s = do
  opts <- get
  do case opts of
       -- Currently, we only have one configuration option, and it's
       -- the file path, so we just ignore it, and then set the new
       -- one.
       Config p _ -> put $ Config p s

-- | Gets the prompt from the `ReplState`.
getPromptStr :: ReplState String
getPromptStr = get >>= (return . promptStr)

-- | The Repl banner with the version number from the global `version`
--   variable.
banner :: String
banner = owlUnicode ++ " Welcome to OwlCore " ++ version ++ " " ++ owlUnicode

-- | The Repl main loop in `ReplState`.
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
               Just "" -> innerLoop config
               Just input -> do                 
                 outputStrLn . (PPrint.pprint) $ Parser.parseCore input
                 innerLoop config

-- | The main function of the repl. `Main.main` simply calls this
--   function.
runRepl :: IO ()
runRepl = evalStateT repl initConfig
