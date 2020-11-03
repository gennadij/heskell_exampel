module Lib
    (calcExactRoot) where

calcExactRoot :: Int -> Int
calcExactRoot radicand = radicand


-- beispiel 50 
-- berchne Ungerade Zahlen
calcOdd :: Int -> [Int]
calcOdd 0 = []
calcOdd x = filter odd [1 .. x]

-- berechne alle moegliche Wurzeln bis zu radicand
-- radicand -> Liste ungerade Zahlen -> Liste mit Wurzeln
calcStandartRoots :: Int -> [Int] -> [Int]
calcStandartRoots radicand (x:y:xs)
    | summe <= radicand = summe : calcStandartRoots radicand (summe : xs)
    | otherwise        = []
    where summe = x + y
-- suche den radicand in der Liste Standardswurzeln

simpleSerchInStandartRoots :: Int -> [Int] -> [Int]
simpleSerchInStandartRoots radicand [] = []
simpleSerchInStandartRoots radicand xs = filter (\x -> radicand == x) xs 

test1 :: Int -> [Int]
test1 radicand = calcStandartRoots radicand (calcOdd radicand)
test2 :: Int -> [Int]
test2 radicand = simpleSerchInStandartRoots radicand (calcStandartRoots radicand (calcOdd radicand))

