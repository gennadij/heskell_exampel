module Main where

import Lib

main :: IO ()
main = do 
  putStrLn "Geben Sie Zahl unter Wurzel ein : "
  radicand <- getLine
  putStrLn (calcExactRoot (read radicand :: Int ))
