module Lib
    (calcExactRoot) where

data Root = Root Int Int deriving (Eq, Show)

getFirst :: Root -> Int
getFirst (Root a b) = a

getSecond :: Root -> Int
getSecond (Root a b) = b 

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

complexSerchOfExactResult :: Int -> [Root] -> [Root]
complexSerchOfExactResult radicand [] = [Root 0 radicand] 
complexSerchOfExactResult radicand (x:xs) 
                            | (snd resFromQout) == 0 = [x, Root (fst resFromQout) (snd resFromQout)]
                            | (snd resFromQout) > 0  = complexSerchOfExactResult radicand xs
                            | otherwise              = [x] 
                            where resFromQout = quotRem radicand (root x)
                                                where root :: Root -> Int
                                                      root (Root _ b) = b

showResult :: Int -> [Root] -> String
showResult radicand [x] = "Ergebnis von sqrt(" ++ show radicand ++ ") ist " ++ show (getFirst x) ++ "."
showResult radicand [x,y] = "Ergebnis von sqrt(" ++ show radicand ++ ") ist " ++ show (getFirst x) ++ "*sqrt(" ++ show (getFirst y) ++ ")."

calcExactRoot :: Int -> String
calcExactRoot radicand
                | result == [] = showResult radicand (complexSerchOfExactResult radicand (giveRoots radicand)) -- versuche den wurzel restlos mit jedem standart Wurzel zu teilen
                | otherwise = showResult radicand (result)
                where result = simpleSerchInStandartRoots radicand (giveRoots radicand)

test4 radicand
                | result == [] = showResult radicand (complexSerchOfExactResult radicand (giveRoots radicand)) -- versuche den wurzel restlos mit jedem standart Wurzel zu teilen
                | otherwise = showResult radicand (result)
                where result = simpleSerchInStandartRoots radicand (giveRoots radicand)
