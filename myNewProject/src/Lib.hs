module Lib
    (calcExactRoot) where

calcExactRoot :: Int -> Int
calcExactRoot radicand = radicand


-- beispiel 50 
-- berchne Ungerade Zahlen
calcOdd :: Int -> [Int]
calcOdd 0 = []
calcOdd x = filter odd [1 .. x]


calcStandartRoots :: [Int] -> [Int]
calcStandartRoots (x:y:xs) = x + y : calcStandartRoots ((x + y) : xs)
calcStandartRoots _ = []

calcStandartRoots' :: Int -> [Int] -> [Int]
calcStandartRoots' radicand (x:y:xs)
    | summe < radicand = summe calcStandartRoots radicand (summe : xs)
    | otherwise        = []
    where summe x y = x + y

test :: Int -> [Int]
test radicand = calcStandartRoots (calcOdd radicand)


-- berechne alle moegliche Wurzeln bis zu radicand

-- 1 + 2 

