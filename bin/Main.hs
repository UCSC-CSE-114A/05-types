import           Control.Monad.Trans
import           System.Console.Haskeline
import qualified Language.Nano.Types     as Nano
import qualified Language.Nano.Eval      as Nano
import qualified Language.Nano.TypeCheck as Nano
import  Control.Exception

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "nano> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just ":q" -> outputStrLn "Goodbye."
      Just (':':'t':' ':input) -> liftIO (typeOf input) >> loop
      Just input -> liftIO (exec input) >> loop

exec :: String -> IO ()
exec s = (print =<< Nano.execString s) `catch` (putStr . Nano.errMsg)

typeOf :: String -> IO ()
typeOf s = (print =<< Nano.typeOfString s) `catch` (putStr . Nano.errMsg)
