initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)  
--infixr 5 :-:
--data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord) 

prefixFunc :: Int -> Int -> Int
prefixFunc a b = a + b 
 
(infixFunc) :: Int -> Int -> Int 
(infixFunc) a  b = a + b