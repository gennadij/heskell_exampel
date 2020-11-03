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

-- 1 + 2 :

calcStandartRoots :: Int -> [Int] -> [Int]
calcStandartRoots radicand (x:y:xs)
    | summe < radicand = summe : calcStandartRoots radicand (summe : xs)
    | otherwise        = []
    where summe = x + y

test :: Int -> [Int]
test radicand = calcStandartRoots radicand (calcOdd radicand)

