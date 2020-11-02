module Main where

import Lib

main :: IO ()
main = do 
  putStrLn "Geben Sie Zahl unter Wurzel ein : "
  zahl <- getLine
  putStrLn ("Die Zahl : " ++ calcExactRoot ++ " .")
