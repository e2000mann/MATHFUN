----------------------------------------------
-- 887818 Functional Programming Coursework --
----------------------------------------------

------------------------------------------------
-- Preparing imports and predefined variables --
------------------------------------------------

-- Imports for higher order functions & data types
-- Control
import Control.Monad
-- Text
import Text.Printf
-- Data
import Data.Char
import Data.List
import Data.Maybe

-- Custom algebraic type Place
-- explanations.txt 1, 2
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

------------------------
-- Core Functionality --
------------------------

-- i
placeNames :: [Place] -> [String]
placeNames placeData = [location p | p <- placeData]

-- find information on place by name
findInfo :: String -> [Place] -> Place
findInfo name (p:ps)
  | name == (location p) = p
  | otherwise = findInfo name ps

-- ii
avgRainfall :: Place -> Float
avgRainfall p = sum(weather p) / 7

avgRainfallIn :: String -> [Place] -> Float
avgRainfallIn name placeData = avgRainfall p
  where p = findInfo name placeData

-- iii
-- explanations.txt 3
weatherToString :: [Float] -> String
weatherToString weather = intercalate ", " . map show $ weather

placeToString :: Place -> String
placeToString p = printf "Location: %-*s Weather: %s\n" (10::Int) (location p) weatherString
  where weatherString = weatherToString (weather p)

allPlacesToString :: [Place] -> String
allPlacesToString [] = ""
allPlacesToString (p:ps) = placeToString p ++ allPlacesToString ps
-- note to self: only formats on new lines when using putstr
-- this is because using show ignores \n

-- iv
wasDryXDaysAgo :: Int -> Place -> Bool
wasDryXDaysAgo x (Place _ _ _ weather) = weather!!(x-1) == 0

allDryPlaces :: Int -> [Place] -> [Place]
allDryPlaces x placeData = filter (\p -> wasDryXDaysAgo x p) placeData

namesOfAllDryPlaces :: Int -> [Place] -> [String]
namesOfAllDryPlaces x placeData = placeNames(allDryPlaces x placeData)

-- v
updateWeather :: Float -> Place -> Place
updateWeather x p = Place (location p) (lat p) (long p) (x: take 6 (weather p))

updateAllWeather :: [Float] -> [Place] -> [Place]
updateAllWeather [] [] = []
updateAllWeather (x:xs) (p:ps) = updateWeather x p : updateAllWeather xs ps

-- vi
removeInfo :: Place -> [Place] -> [Place]
removeInfo old placeData = [ p | p <- placeData, p /= old]

replaceLocation :: Place -> Place -> [Place] -> [Place]
replaceLocation old new placeData = new : (removeInfo old placeData)

-- vii
findDist :: Float -> Float -> Place -> Float
-- n = north w = west
findDist n w p = sqrt ( (n - long p) ^ 2 + (w - lat p) ^ 2)

dryPlaceDists :: Float -> Float -> [Place] -> [Float]
dryPlaceDists n w placeData = [findDist n w p | p <- dryPlaces]
  where dryPlaces = allDryPlaces 1 placeData

closestDryPlaceIndex :: Float -> Float -> [Place] -> Int
closestDryPlaceIndex n w placeData = fromMaybe 0 (elemIndex (minimum(dists)) dists)
  where dists = dryPlaceDists n w placeData

closestDryPlace :: Float -> Float -> [Place] -> Place
closestDryPlace n w placeData = (allDryPlaces 1 placeData) !! index
  where index = closestDryPlaceIndex n w placeData

-- rainfall map
type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text

latAvg :: [Place] -> Float
latAvg placeData = sum(allLat) / fromIntegral (length placeData)
  where allLat = [lat p | p <- placeData]

longAvg :: [Place] -> Float
longAvg placeData = sum(allLong) / fromIntegral (length placeData)
  where allLong = [long p | p <- placeData]

getXTrans :: Float -> Int
getXTrans diff
  | diff > 0.1 = 1 + getXTrans (diff - 0.1)
  | diff < -0.1 = (-1) + getXTrans (diff + 0.1)
  | otherwise = 0

getYTrans :: Float -> Int
getYTrans diff
  | diff > 0.2 = 1 + getYTrans (diff - 0.2)
  | diff < -0.2 = (-1) + getYTrans (diff + 0.2)
  | otherwise = 0

xScreenCoord :: Float -> Float -> Int
xScreenCoord lat avgLat = 40 + getXTrans (lat - avgLat)

yScreenCoord :: Float -> Float -> Int
yScreenCoord long avgLong = 25 + getYTrans (long - avgLong)

getLocOnScreen :: Place -> Float -> Float -> ScreenPosition
getLocOnScreen p avgLat avgLong = (x, y)
  where x = xScreenCoord (lat p) avgLat
        y = yScreenCoord (long p) avgLong

getAllPositions :: [Place] -> [ScreenPosition]
getAllPositions placeData = [getLocOnScreen p avgLat avgLong | p <- placeData]
  where avgLat = latAvg placeData
        avgLong = longAvg placeData

rainfallMap :: [Place] -> [IO()]
rainfallMap placeData = [writeAt position "+" | position <- getAllPositions placeData]

-------------------
-- Demo function --
-------------------

demo :: Int -> IO ()
-- display the names of all the places
demo 1 = putStrLn (show (placeNames testData))
-- display, to two decimal places, the average rainfall in Cardiff
demo 2 = putStrLn (printf "%.2f" (avgRainfallIn "Cardiff" testData))
demo 3 = putStrLn (allPlacesToString testData)
-- -- display the names of all places that were dry two days ago
demo 4 = putStrLn (show (placeNames (allDryPlaces 2 testData)))
-- update the data with most recent rainfall
--[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 5 = putStrLn (allPlacesToString newData)
  where
    newData = updateAllWeather [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData
-- replace "Plymouth" with "Portsmouth" which has
-- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 6 = putStrLn (allPlacesToString newData)
  where
    newData = replaceLocation plymouth portsmouth testData
    plymouth = findInfo "Plymouth" testData
    portsmouth = (Place "Portsmouth" 50.8 (-1.1) [0, 0, 3, 2, 5, 2, 1])
-- display the name of the place closest to 50.9 (N), -1.3 (E)
-- that was dry yesterday
demo 7 = putStrLn (location closest)
  where
    closest = closestDryPlace 50.9 (-1.3) testData
-- demo 8 = -- display the rainfall map

------------------------------
-- User Interface & File IO --
------------------------------

-- other IO functions
getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

getFloat :: IO Float
getFloat = do
  str <- getLine
  return (read str :: Float)

getWeatherArray :: IO [Float]
getWeatherArray = do
  str <- getLine
  return (read str :: [Float])

-- menu IO

loadMenu :: IO Char
loadMenu = do
  putStrLn "Please chose one of the following options:\n\
  \1) Return all location names \n\
  \2) Return the average rainfall of a location\n\
  \3) Display all locations & the previous week's rainfall\n\
  \4) Return locations that were dry a certain number of days ago\n\
  \5) Add today's weather\n\
  \6) Replace a location\n\
  \7) Get the closest location that was dry yesterday\n\
  \8) Draw a rainfall map\n\
  \9) Exit the program"
  userInput <- getChar
  if userInput `elem` ['1' .. '9']
    then return userInput
    else do
      putStrLn "That is not a valid input! Try again..."
      loadMenu

runMenu :: [Place] -> IO()
runMenu placeData =
  forever $ do
    userInput <- loadMenu
    case userInput of
      -- return names of places
      '1' -> putStrLn (placeNames placeData)
      -- return average rainfall of place
      '2' -> putStrLn (option2 placeData)
      -- return all names & week weather
      '3' -> putStrLn (allPlacesToString placeData)
      -- return dry places x days ago
      '4' -> putStrLn (option4 placeData)
      -- update weather
      '5' -> putStrLn (option5 placeData)
      -- update location
      '6' -> putStrLn (option6 placeData)
      -- return closest dry place
      '7' -> putStrLn (option7 placeData)
      -- rainfall map
      '8' -> writeToFile placeData
      -- exit
      '9' -> writeToFile placeData

-- menu IO : option code

checkLocExists :: String -> [Place] -> Bool
checkLocExists str placeData = str `elem` locations
  where
    locations = [location p | p <- placeData]

option2 :: [Place] -> IO String
option2 placeData = do
  putStr "Which location?: "
  location <- getLine
  locationExists <- checkLocExists location placeData
  if locationExists
    then return (printf "%.2f" (avgRainfallIn location placeData))
    else
      putStrLn "That is not a valid location, try again..."
      return option2

option4 :: [Place] -> IO String
option4 placeData = do
  putStr "How many days ago?: "
  days <- getInt
  if days `elem` [1 .. 7]
    then return (show placeNames (allDryPlaces days placeData))
    else
      putStrLn "We do not have data for that many days ago. Try again..."
      return option4

option5 :: [Place] -> IO String
option5 placeData = do
  putStrLn "Type in weather array for today: "
  weatherArray <- getWeatherArray
  if (length weatherArray) == 14
    then
      return (allPlacesToString (updateAllWeather weatherArray placeData))
  else
    putStrLn "The weather array was not correct. Try again..."
    return option5

getPlace :: IO Place
getPlace = do
  putStrLn "Type in the location's name: "
  name <- getLine
  putStrLn "Type in the location's longitude: "
  n <- getFloat
  putStrLn "Type in the location's latitude: "
  e <- getFloat
  putStrLn "Type in the last week's rainfall for this location: "
  weather <- getWeatherArray
  newPlace <- Place name n e weather
  return newPlace

option6 :: [Place] -> IO String
option6 placeData = do
  putStrLn "What location are you replacing?: "
  old <- getLine
  oldExists <- checkLocExists old placeData
  if oldExists
    then do
      oldPlace <- findInfo old placeData
      putStrLn "Now please type in the information for the new location..."
      newPlace <- getPlace
      return (allPlacesToString (replaceLocation oldPlace newPlace placeData))
    else
      putStrLn "That location doesn't exist! Try again..."
      return option6

option7 :: [Place] -> IO String
option7 placeData = do
  putStrLn "What is your longitude?: "
  n <- getFloat
  putStrLn "What is your latitude?: "
  e <- getFloat
  closest <- closestDryPlace n e placeData
  return (location closest)

-- file IO
loadFromFile :: IO [Place]
loadFromFile = do
  placeData <- readFile "places.txt"
  return (read placeData :: [Place])

writeToFile :: [Place] -> IO()
writeToFile placeData = do
  writeFile "places.txt" (show placeData)

-- main function
main :: IO()
main = do
  placeData <- loadFromFile
  putStrLn (placeNames placeData)
  runMenu placeData
