--up887818

--1
timesTen :: Int -> Int
timesTen x = x * 10

--2
sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

--3
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r * r

--4
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r h = (areaOfCircle r) * h

--5
distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt((y1 - y2) ^ 2 + (x1 - x2) ^ 2)

--6
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = (x /= y) && (x /= z) && (y /= z)

--7
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0
  --backticks are necessary!!
  --alternative: mod(x,y)

--8
isEven :: Int -> Bool
isEven x = divisibleBy x 2

--9
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral(sumThree x y z) / 3

--10
absolute :: Int -> Int
absolute x = if x < 0 then -x else x
