-- up887818

-- Imports
import Data.List

-- Functional Code
-- 1
addWord :: String -> [String] -> [String]
addWord str strs = strs ++ [str]

-- 2
wordsToString :: [String] -> String
wordsToString strs = intercalate "\n" strs

-- 3
wordsOfLength :: Int -> [String] -> [String]
wordsOfLength x strs = filter (\str -> length str == x) strs

-- User Interface Code
main :: IO ()
main = do
  input <- readFile "words.txt"
  putStr show (lines input)
