
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type
data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String |
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null |
     Node Int Tree Tree
     deriving (Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2

-- 887818
-- 1
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
            deriving (Eq,Ord,Show,Read)

data Season = Spring | Summer | Autumn | Winter
              deriving (Eq,Ord,Show,Read)

-- 2
season :: Month -> Season
season month
  | month `elem` [Dec, Jan, Feb] = Winter
  | month `elem` [Mar, Apr, May] = Spring
  | month `elem` [Jun, Jul, Aug] = Summer
  | otherwise = Autumn

-- 3
-- isLeap :: Int -> Bool
-- isLeap year = (year `mod` 4) == 0
numberOfDays :: Month -> Int -> Int
numberOfDays month year
  | month == Feb && (year `mod` 4) == 0 = 29
  | month == Feb = 28
  | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
  | otherwise = 30

-- 4
data Point = Point Float Float
             deriving (Eq,Ord,Show,Read)

-- 5
data PositionedShape = PositionedShape Shape Point

-- 6
move :: PositionedShape -> Float -> Float -> PositionedShape
move (Circle r, Point x y) x2 y2 = 
