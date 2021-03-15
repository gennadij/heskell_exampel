initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  
--infixr 5 :-:
--data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 

prefixFunc :: Int -> Int -> Int
prefixFunc a b = a + b 
 
(+++++) :: Int -> Int -> Int 
(+++++) a  b = a + b

data TrafficLight = Red | Yellow | Green 

instance Eq TrafficLight where 
  Red    == Red    = False
  Yellow == Yellow = False
  Green  == Green  = False
  _      == _      = True

instance Show TrafficLight where 
  show Red    = "Rotes Licht"
  show Green  = "Grünes Licht"
  show Yellow = "Gelbes Licht"

-- data ZipList a = ZipList [a] 
data ZipList a = ZipList { getZipList :: [a] } 
data Profession = Fighter | Archer | Accountant 
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)
instance Functor (Pair c) where  
  fmap f (Pair (x, y)) = Pair (f x, y)
  fmap2 f 