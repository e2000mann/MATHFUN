{- Week4.hs
 Matthew Poole
 This module illustrates strings, tuples and lists
-}

module Week4 where
import Char

type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

-- SOLUTIONS 

-- 1
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

--2
grade :: StudentMark -> Char
grade (_,mark) 
    | mark >= 70    = 'A'
    | mark >= 60    = 'B'
    | mark >= 50    = 'C'
    | mark >= 40    = 'D'
    | otherwise     = 'F'

--3
capMark :: StudentMark -> StudentMark 
capMark (st,mk) 
    | mk >= 40      = (st,40)
    | otherwise     = (st,mk)

--4
firstNumbers :: Int -> [Int]
firstNumbers n = [1 .. n]

--5
firstSquares :: Int -> [Int]
firstSquares n = [ i ^ 2  | i <- [1 .. n] ]

-- 6
capitalise :: String -> String
capitalise str = [ toUpper c | c <- str ]

-- 7
onlyDigits :: String -> String
onlyDigits str = [ c | c <- str, isDigit c]

-- 8 
capMarks :: [StudentMark] -> [StudentMark]
capMarks stList = [capMark stmk | stmk <- stList ]

-- 9
gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents stList = [(st,grade (st,mk)) | (st,mk) <- stList ]

-- 10
duplicate :: String -> Int -> String
duplicate str n 
    | n == 0        = ""
    | n > 0         = str ++ duplicate str (n - 1)

-- 11
divisors :: Int -> [Int]
divisors n = [ i | i <- [ 1 .. n ], mod n i == 0 ]

-- 12
isPrime :: Int -> Bool
isPrime n = divisors n == [1, n]

-- 13
split :: [(a,b)] -> ([a],[b])
split lst = ([ x | (x, _) <- lst] , [ y | (_, y) <- lst])
