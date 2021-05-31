cup m = \message -> message m

getM aCup = aCup (\m -> m)

drink aCup ozDrank = cup (m - ozDrank)
  where m = getM aCup


lambda num = \num -> num

