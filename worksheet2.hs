-- up887818

-- 1
absolute :: Int -> Int
absolute x
  | x < 0 = -x
  | otherwise = x

-- 2
sign :: Int -> Int
sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1

-- 3
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z = 3
  | x == y || x == z || y == z = 2
  | otherwise = 0

-- 4
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = diagonalLength x + diagonalLength y + diagonalLength z
                         where
                           diagonalLength a = a * sqrt(2)

-- 5
taxiFare :: Int -> Float
taxiFare km = 2.20 + extra km
  where
    extra km
      | km <= 10 = fromIntegral(km) * 0.5
      | otherwise = extra 10 + fromIntegral(km - 10) * 0.3

-- 6
howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
  | x > average && y > average = 2
  | x > average && z > average = 2
  | y > average && z > average = 2
  -- && = and, || = or
  | x > average || y > average || z > average = 1
  -- can't have all 3 numbers above average
  | otherwise = 0
  where
    average = (x + y + z) `div` 3

-- 7
validDate :: Int -> Int -> Bool
validDate d m
  | d <= 0 = False
  | d <= daysInMonth m = True
  | otherwise = False
  where
    daysInMonth m
      | m > 12 = 0
      -- m is not a valid month
      | m == 2 = 28
      -- m is february
      | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
      -- m is a month with 31 days (`elem` checks if in list)
      | otherwise = 30

daysInMonth :: Int -> Int -> Int
daysInMonth m y
  | m == 2 && y `mod` 4 == 0 = 29
  -- m is february in a leap year
  | m == 2 = 28
  -- m is february in a non-leap year
  | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
  -- m is a month with 31 days
  | otherwise = 30
