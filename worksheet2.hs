absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

sign :: Int -> Int
sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z = 3
  | x == y || x == z || y == z = 2
  | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagonalLength x + diagonalLength y + diagonalLength z
                         where
                           diagonalLength a = a * sqrt(2)

taxiFare :: Int -> Float
taxiFare km
  | km == 0 = 2.20
  | km <= 10 = 2.20 + km * 0.50
  | otherwise = 2.20 + 5 + (km - 10) * 0.30
-- need to use where in this??

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | x > average && y > average = 2
  | x > average && z > average = 2
  | y > average && z > average = 2
  | x > average = 1
  | y > average = 1
  | z > average = 1
  | otherwise = 0
  | where
    average x y z = (x + y + z) / 3
