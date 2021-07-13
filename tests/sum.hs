import System.Environment
import Control.Monad
main :: IO ()
main = do
  userInput <- getContents
  mapM_ print userInput

