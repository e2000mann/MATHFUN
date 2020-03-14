module Week4 where
import Data.Char

type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

grade :: StudentMark -> Char
grade (name, mark)
  | mark >= 70 = 'A'
  | mark >= 60 = 'B'
  | mark >= 50 = 'C'
  | mark >= 40 = 'D'
  | otherwise = 'F'

capMark :: StudentMark -> StudentMark
capMark (name, mark)
  | mark <= 40 = (name, mark)
  | otherwise = (name, 40)

firstNumbers :: Int -> [Int]
firstNumbers x = [1 .. x]

firstSquares :: Int -> [Int]
firstSquares x = [i ^ 2 | i <- firstNumbers x]

capitalise :: String -> String
capitalise str = [toUpper l | l <- str]

onlyDigits :: String -> String
onlyDigits str = [l | l <- str, isDigit l]

capMarks :: [StudentMark] -> [StudentMark]
capMarks group = [capMark stu | stu <- group]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents group = [(stu, grade (stu, mark)) | (stu, mark) <- group]

duplicate :: String -> Int -> String
duplicate str 1 = str
duplicate str x = str ++ duplicate str (x-1)

divisors :: Int -> [Int]
divisors n = [x | x <- [1 .. n], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime x
  | length [divisors x] <= 2 = True
  | otherwise = False

split :: [(a, b)] -> ([a], [b])
split lst = ([ x | (x, _) <- lst] , [ y | (_, y) <- lst])
