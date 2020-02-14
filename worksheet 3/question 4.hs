daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth m | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
daysInMonth _ = 30

validDate :: Int -> Int -> Bool
validDate d m = d <= daysInMonth m
