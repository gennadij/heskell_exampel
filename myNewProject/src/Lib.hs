module Lib
    (calcExactRoot) where

data Root = Root Int Int deriving (Eq, Show)

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

appendResultOnStandartRoots :: [Int] -> [Int] -> [Root]
appendResultOnStandartRoots [] _ = []
appendResultOnStandartRoots _ [] = []
appendResultOnStandartRoots (x:xs) (y:ys) = Root x y : appendResultOnStandartRoots xs ys

giveRoots :: Int -> [Root]
giveRoots radicand = appendResultOnStandartRoots [2 .. radicand] (calcStandartRoots radicand (calcOdd radicand))

simpleSerchInStandartRoots :: Int -> [Root] -> [Root]
simpleSerchInStandartRoots radicand [] = []
simpleSerchInStandartRoots radicand xs = filter (\(Root a b) -> radicand == b) xs

complexSerchOfExactResult :: Int -> [Root] -> (Int, Int)
complexSerchOfExactResult radicand (x:xs) 
                            | (snd resFromQout) == 0 = complexSerchOfExactResult radicand xs
                            | (snd resFromQout) > 0  = (0,0)
                            where resFromQout = quotRem radicand (root x)
                                                where root :: Root -> Int
                                                      root (Root _ b) = b

test4 radicand
                | result == [] = [] -- versuche den wurzel restlos mit jedem standart Wurzel zu teilen
                | otherwise = result
                where result = simpleSerchInStandartRoots radicand (giveRoots radicand)

test3 radicand = simpleSerchInStandartRoots radicand (giveRoots radicand)
