{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char
import Data.List
import Data.Maybe

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- 1
mult10 :: [Int] -> [Int]
mult10 xs = map (*10) xs

-- 2
onlyLowerCase :: String -> String
onlyLowerCase str = filter isLower str

-- 3
orAll :: [Bool] -> Bool
orAll xs = foldr (||) False xs

-- 4
-- squareAll :: [Int] -> [Int]
-- squareAll xs = map (^2) xs
sumSquares :: [Int] -> Int
sumSquares xs = foldr (+) 0 (map (^2) xs)

-- 5
zeroToTen :: [Int] -> [Int]
zeroToTen xs = filter (<= 10) (filter (>=0) xs)

-- 6
-- positivesOnly :: [Float] -> [Float]
-- positivesOnly xs = filter (>=0) xs
squareRoots :: [Float] -> [Float]
squareRoots xs = map (sqrt) (filter (>=0) xs)

-- 7
countBetween :: Float -> Float -> [Float] -> Int
countBetween a b xs = length (filter (>=a) (filter (<=b) xs))

-- 8
alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive x ys = (filter (>0) (map (x) ys)) == map (x) ys
-- map does the given function to all elements in list
-- filter removes negative values
-- == checks to see if filter changes anything vs just map
-- alternative: all (>0) (map (x) ys)

-- 9
productSquareRoots :: [Float] -> Float
productSquareRoots xs = foldr (*) 1 (map (sqrt) (filter (>0) xs))
-- filter removes negative values from list
-- map square roots remaining values
-- foldr multiplies together

-- 10
removeFirst :: (a -> Bool) -> [a] -> a
removeFirst test xs = fromJust(find test xs)

-- 11
-- removeLast :: (a -> Bool) -> [a] -> [a]

-- 12
zeroToTen' :: [Int] -> [Int]
zeroToTen' xs = filter (\x -> x>=0 && x<=10) xs

-- 13
---iii
onlyLowerCase' :: String -> String
onlyLowerCase' xs = foldr (:) (\x -> if isLower x == True then x) xs
