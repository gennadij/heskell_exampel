type Events = [String]
type Probs = [Double]

data PTable = PT Events Probs

createPTable :: Events -> Probs -> PTable
createPTable es ps = PT es normalizedProbs
  where totalProbs = sum ps 
        normalizedProbs = map (\x -> x / totalProbs) ps

showPair :: String -> Double -> String
showPair e p = mconcat [e, " | ", show p, "\n"]

instance Show PTable where 
  show (PT events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd     = length l2
        repitedL1  = map (take nToAdd . repeat) l1
        newL1      = mconcat repitedL1
        cycledL2   = cycle l2 

combEvents :: Events -> Events -> Events
combEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x, " - ", y])

combProbs :: Probs -> Probs -> Probs
combProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
  (<>) ptable1 (PT [] []) = ptable1
  (<>) (PT [] []) ptable2 = ptable2
  (<>) (PT e1 p1) (PT e2 p2) = createPTable newEvents newProbs
      where newEvents = combEvents e1 e2
            newProbs = combProbs p1 p2

instance Monoid PTable where
  mempty = PT [] []
  mappend = (<>)

