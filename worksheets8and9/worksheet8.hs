-- up887818

-- imports
import Text.Printf
import Control.Monad

-- from examples
getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

-- 1
greeting :: IO ()
greeting = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name)

-- 2
addTwoNumbers :: IO ()
addTwoNumbers = do
  putStrLn "First number:"
  x <- getInt
  putStrLn "Second number:"
  y <- getInt
  putStrLn (printf "%d + %d = %d" x y (x+y))

-- 3
copyFile :: IO ()
copyFile = do
  -- get old filename & contents
  putStr "Enter old filename: "
  name <- getLine
  contents <- readFile name
  -- get new filename and write
  putStr "Enter new filename: "
  newName <- getLine
  writeFile newName contents

-- 4
buildList :: [String] -> IO()
buildList strs = do
  putStr "Enter a line: "
  str <- getLine
  when (str /= "") $ do
    strs <- strs ++ [str]
    putStr "List is now " ++ show strs
    buildList strs

-- 5
writeAndSumInt :: Int -> Int -> Int -> IO Int
writeAndSumInt x y ySum = do
  putStrLn (show x)
  if x < y
    then writeAndSumInt (x+1) y (ySum+x)
    else return (ySum+x)

readInt :: IO ()
readInt = do
  putStr "Give an integer: "
  y <- getInt
  ySum <- writeAndSumInt 1 y 0
  putStrLn (printf "Sum: %d" ySum)
