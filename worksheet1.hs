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
threeDifferent x y z = if (x /= y) && (y /= z) && (x /= z) then True else False
  -- if (x doesn't equal y) and (y doesn't equal z) and (x doesn't equal z)

--7
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = if (x `mod` y) == 0 then True else False
  --backticks are necessary!!

--8
isEven :: Int -> Bool
isEven x = divisibleBy x 2

--9
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral(sumThree x y z) / 3

--10
absolute :: Int -> Int
absolute x = if x < 0 then -x else x
