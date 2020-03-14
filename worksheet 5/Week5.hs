{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

module Week4 where
import Data.Char
import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

type StudentMark = (String, Int)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

headPlusOne :: [Int] -> Int
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead (x:xs) = x:x:xs

rotate :: [a] -> [a]
rotate (x:y:xs)
  | length xs > 0 = y:x:xs
  | otherwise = x:y:xs

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs
-- alt
-- andAll xs = foldr (&&) True xs

countElems :: Int -> [Int] -> Int
countElems n [] = 0
countElems n (x:xs) = fromEnum(x == n) + countElems n xs
-- fromEnum :: Enum a => a -> Int
-- for bool returns 1 for True & 0 for False

-- removeAll :: Int -> [Int] -> [Int]
-- removeAll n (x:xs)
--   | n /= x && null xs = [x]
--   | n == x && null xs = []
--   | n == x = removeAll n xs
--   | otherwise = x : removeAll n xs

-- listMarks :: String -> [StudentMark] -> [Int]
-- listMarks name (mark:marks)
--   |

sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:ys) = (x <= y) && sorted (y:ys)

prefix :: [Int] -> [Int] -> Bool
prefix (x:xs) (y:ys)
  | null (x:xs) = True
  | x == y = True && prefix xs ys
  | otherwise = False
