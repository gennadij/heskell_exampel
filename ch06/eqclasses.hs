class BasicEq a where 
  isEqual :: a -> a -> Bool

instance BasicEq Bool where 
  isEqual True True   = True
  isEqual False False = true
  isEqual _ _         = False