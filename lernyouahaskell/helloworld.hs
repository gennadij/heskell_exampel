import Data.Char

main = do 
	putStrLn "Wie heißt du?"
	name <- getLine
	let bigName = map toUpper name
	putStrLn $ "Deine Name Großgeschrieben " ++ bigName ++ "."
