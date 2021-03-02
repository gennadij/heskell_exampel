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