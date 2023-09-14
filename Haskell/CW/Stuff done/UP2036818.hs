--
-- MATHFUN
-- UP2036818
-- 
--

--
-- Imports
import Data.Char
import Data.List
import System.IO
import Text.Printf
-- import Text.PrettyPrint
import Data.Maybe
--
-- Types (define your Station type here)
-- type StationName = String
-- type StationCoordinates = (Float, Float)
-- type StationTemperatures = [Float]


data Station = Station String (Float, Float) [Float]
    deriving Show

-- data Station = Station {name :: String, coordinates :: (Float, Float), temperatures :: [Float]} deriving Show
data Months = January | February | March | April | May | June | July | August | September | October | November | December deriving (Show, Enum)



testData :: [Station]
testData =  [(Station "Mumbles Head" (51.565, -3.981) [8.26, 8.33, 9.84, 12.36, 15.24, 17.83, 19.55, 19.67, 17.97, 14.70, 11.49, 9.09]),
            (Station "Greenwich Park" (51.477, 0.004) [8.47, 9.21, 12.07, 15.35, 18.59, 21.37, 23.75, 23.31, 20.29, 15.83, 11.55, 8.85]),
            (Station "Solent" (50.807, -1.208) [8.56, 8.74, 11.01, 13.94, 17.07, 19.59, 21.62, 21.61, 19.38, 15.73, 11.88, 9.17]),
            (Station "Ronaldsway" (54.085, -4.632) [8.47, 8.35, 9.44, 11.48, 14.33, 16.52, 18.19, 18.15, 16.56, 13.83, 11.10, 9.17]),
            (Station "Baltasound" (60.749, -0.850) [6.55, 6.32, 7.35, 9.16, 11.20, 13.25, 15.08, 15.39, 13.62, 10.88, 8.47, 7.00]),
            (Station "St Austell" (50.337, -4.787) [9.46, 9.65, 11.33, 13.30, 16.18, 18.10, 20.60, 20.36, 18.54, 14.99, 12.30, 10.18]),
            (Station "Heathrow" (51.479, -0.449) [8.42, 8.98, 11.73, 15.00, 18.37, 21.57, 23.89, 23.40, 20.22, 15.81, 11.47, 8.79]),
            (Station "Hunstanton" (52.939, 0.493) [7.05, 7.45, 9.77, 12.65, 15.96, 18.84, 21.34, 21.28, 18.32, 14.46, 10.29, 7.56]),
            (Station "Durham" (54.767, -1.583) [6.86, 7.75, 9.87, 12.49, 15.42, 17.96, 20.24, 19.87, 17.36, 13.51, 9.65, 7.07]),
            (Station "Monks Wood" (52.400, -0.233) [7.58, 8.36, 11.05, 14.14, 17.19, 20.01, 22.63, 22.49, 19.50, 15.18, 10.68, 7.85])]   
-- --
-- --  Your functional code goes here
-- --

--Get Station name
getName :: Station -> String            
getName (Station name _ _) = name

--Takes list of stations and returns list of strings
showStations :: [Station] -> [String]
showStations [] = []
showStations ((Station name _ _):xs) = name : showStations xs

--Add station to testData
addStation :: Station -> [Station] -> [Station]
addStation station [] = [station]
addStation station (x:xs) = x : addStation station xs

--Converts list of Celcius to list of Fahrenheit
celciusToFahrenheit :: [Float] -> [Float]
celciusToFahrenheit [] = []
celciusToFahrenheit (x:xs) = x * 1.8 + 32 : celciusToFahrenheit xs

--Takes stations temperatures and outputs them after converting to Fahrenheit
convertedTempsStation :: [Station] -> [Station]
convertedTempsStation [] = []
convertedTempsStation ((Station name coords tempC):xs) = (Station name coords (celciusToFahrenheit tempC)) : convertedTempsStation xs

--Takes a list of stations and outputs them after converting to a single string
stationsToString :: [Station] -> String
stationsToString [] = ""
stationsToString [x] = show x
stationsToString (x:xs) = (show x) ++ "\n\n" ++ stationsToString xs 

--Checks temperature values for the indexed month.
checkTemperature :: [Station] -> Int -> Float -> [Station]
checkTemperature [] _ _= []
checkTemperature ((Station name coords temp):xs) index tempCheck=
    if temp !! index > tempCheck
    then (Station name coords temp) :checkTemperature xs index tempCheck
    else checkTemperature xs index tempCheck

--Replaces a value in a list
replaceValues :: Int -> a -> [a] -> [a]    
replaceValues index newValue list = take index list ++ [newValue] ++ drop (index+1) list

modTemp :: [Station] -> String -> Int -> Float ->Station
modTemp ((Station name coords temp):xs) stationName index newTemp = 
    if name == stationName
    then (Station name coords (replaceValues index newTemp temp))
    else modTemp xs stationName index newTemp

findStationIndex :: [Station] -> String -> Maybe Int
findStationIndex strings name = elemIndex name (showStations strings)

checkTempForMarch :: [Station] -> [Station]
checkTempForMarch [] = []
checkTempForMarch ((Station name coords temps):xs) =
    if temps !! 2 > 10
    then (Station name coords temps) : checkTempForMarch xs
    else checkTempForMarch xs

findMinimum :: [Float] -> Float
findMinimum [] = 0
findMinimum [x] = x
findMinimum (x:xs) = min x (findMinimum xs)

checkDistances :: [Station] -> [Float]
checkDistances [] = []
checkDistances ((Station name (north, east) temp):xs) = sqrt((50.2 - north)**2 + (-0.4 - east)**2) : checkDistances xs

identifyStation :: [Station] -> [Station]
identifyStation [] = []
identifyStation ((Station name (north, east) temp):xs) =
    if sqrt((50.2 - north)**2 + (-0.4 - east)**2) == findMinimum(checkDistances (checkTempForMarch(testData)))
    then (Station name (north, east) temp) : identifyStation xs
    else identifyStation xs

--
--  Demo
-- 

demo :: Int -> IO ()   

--Output the names of all the weather stations
demo 1 = mapM_ putStrLn(showStations testData)     

--Adds station Valey to testData and prints out the whole list
demo 2 = putStrLn(stationsToString(addStation (Station "Valley" (53.252, -4.537) [8.37, 8.44, 9.84, 12.09, 15.01, 17.24, 18.77, 18.76, 17.26, 14.31, 11.26, 9.09]) testData))

-- output the data with all temperature values converted to degrees Fahrenheit
demo 3 = putStrLn(stationsToString(convertedTempsStation testData))

-- output the names of weather stations with August (index 7) temperature warmer than 20 degrees Celsius
demo 4 = putStrLn(stationsToString(checkTemperature testData 7 20))
    
demo 5 = putStrLn (stationsToString testData)

-- output the data after changing the temperature of "Heathrow" for July (index 6) to 25 degrees Celsius
demo 6 = putStrLn(stationsToString (replaceValues (fromMaybe 0 (findStationIndex testData "Heathrow")) (modTemp testData "Heathrow" 6 25) testData))

-- output the name of the nearest weather station to location (50.2N, -0.4E) which has a March temperature warmer than 10 degrees Celsius
--Heathrow
demo 7 =  mapM_ putStrLn(showStations(identifyStation testData))


--demo 8 = -- output an animated bar chart of the temperature figures

-- --
-- -- Screen Utilities (use these to do the bar chart)
-- --

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


-- --
-- -- Your bar chart code goes here
-- --



-- --
-- -- Your user interface (and loading/saving) code goes here
-- --
