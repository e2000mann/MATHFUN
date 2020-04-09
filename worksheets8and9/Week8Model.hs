-- Worksheet 8 selected model solutions

getInt :: IO Int
getInt = do str <- getLine
            return (read str :: Int)

-- Ex 1
greeting :: IO () 
greeting = do  putStr "Enter your name: "
               name <- getLine
               putStr ("Hello, " ++ name)

-- Ex 3
copyFile :: IO ()
copyFile = do putStr "Enter name of file to copy: "
              source <- getLine
              putStr "Enter new file name: "
              target <- getLine
              contents <- readFile source
              writeFile target contents

-- Ex 5
addNum :: Int ->  Int -> IO Int
addNum 0 acc = return acc
addNum n acc = do putStr "Enter next number: "
                  i <- getInt
                  addNum (n - 1) (acc + i)

addNumbers :: IO ()
addNumbers = do putStr "How many numbers? "
                n <- getInt
                s <- addNum n 0
                putStrLn ("The sum is " ++ (show s))
