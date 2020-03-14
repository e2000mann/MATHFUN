-- 887818 Functional Programming CW

-- Imports for higher order functions & data types
import Data.Char
import Data.List

-- Custom algebraic type Place
-- Written in record syntax so each variable is callable, e.g.
-- location x gives the location of Place x
-- location (Place "Plymouth" 50.4 (-4.1) [4, 9, 0, 0, 0, 6, 5]) = "Plymouth"
-- Weather stored as [Float]  (vs [Int]) to make it easier to divide,
-- this would also allow for more accurate weather measurements
data Place = Place { location :: String,
                     long :: Float,
                     lat :: Float,
                     weather :: [Float] }
             deriving (Eq,Ord,Show,Read)

--Defining Test Data
testData :: [Place]
testData =
  [Place "London" 51.5 (-0.1) [0, 0, 5, 8, 8, 0, 0],
  Place "Cardiff" 51.5 (-3.2) [12, 8, 15, 0, 0, 0, 2],
  Place "Norwich" 52.6 1.3 [0, 6, 5, 0, 0, 0, 3],
  Place "Birmingham" 52.5 (-1.9) [0, 2, 10, 7, 8, 2, 2],
  Place "Liverpool" 53.4 (-3.0) [8, 16, 20, 3, 4, 9, 2],
  Place "Hull" 53.8 (-0.3) [0, 6, 5, 0, 0, 0, 4],
  Place "Newcastle" 55.0 (-1.6) [0, 0, 8, 3, 6, 7, 5],
  Place "Belfast" 54.6 (-5.9) [10, 18, 14, 0, 6, 5, 2],
  Place "Glasgow" 55.9 (-4.3) [7, 5, 3, 0, 6, 5, 0],
  Place "Plymouth" 50.4 (-4.1) [4, 9, 0, 0, 0, 6, 5],
  Place "Aberdeen" 57.1 (-2.1) [0, 0, 6, 5, 8, 2, 0],
  Place "Stornoway" 58.2 (-6.4) [15, 6, 15, 0, 0, 4, 2],
  Place "Lerwick" 60.2 (-1.1) [8, 10, 5, 5, 0, 0, 3],
  Place "St Helier" 49.2 (-2.1) [0, 0, 0, 0, 6, 10, 0]]

-- Core Functionality
-- i
placeNames :: [Place] -> [String]
placeNames placeData = [location x | x <- placeData]

-- find information on place by name
findInfo :: String -> [Place] -> Place
findInfo name (x:xs)
  | name == (location x) = x
  | otherwise = findInfo name xs

-- ii
avgRainfall :: Place -> Float
avgRainfall (Place _ _ _ weather) = sum(weather) / 7

avgRainfallOfName :: String -> [Place] -> Float
avgRainfallOfName name placeData = avgRainfall (findInfo name placeData)

-- iii
-- allLocationsAndNames :: [Place] -> String
-- allLocationsAndNames (x:xs) = "Location: " ++ location x ++ " & Weather : " ++ weather x ++ "\n" ++ allLocationsAndNames xs
-- allLocationsAndNames [] = ""

-- iv
wasDryXDaysAgo :: Int -> Place -> Bool
wasDryXDaysAgo x (Place _ _ _ weather) = weather!!(x-1) == 0
