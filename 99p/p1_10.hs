myLast :: [a] -> a
myLast xs
  | null xs = error "No end for empty lists!"
  | length xs == 1 = head xs
  | otherwise = myLast(tail xs)

lastButOne :: [a] -> a
lastButOne xs = head (tail (reverse xs))

elementAt :: [a] -> Int -> a
elementAt xs i
  | i <= 0 = error "List index out of bounds"
  | null xs = error "List index out of bounds"
  | i == 1 = head xs
  | otherwise = elementAt (tail xs) (i - 1)

myLength :: [a] -> Int
myLength xs
  | null xs = 0
  | otherwise = 1 + myLength (tail xs)