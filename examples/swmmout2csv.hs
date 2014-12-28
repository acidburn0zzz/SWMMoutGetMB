-- |
-- Module : Examples.swmmout2csv
-- Copyright : (C) 2014 Siddhanathan Shanmugam
-- License : LGPL (see LICENSE)
-- Maintainer : siddhanathan@gmail.com
-- Portability : very
--
-- Example of a program using SWMMoutGetMB for parsing SWMM .OUT files
-- Inspired by OOW/swmmout2csv originally written by @mplourde
--

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Water.SWMM
import qualified Data.ByteString.Lazy as BL        (ByteString, appendFile, readFile)
import qualified Data.ByteString.Lazy.Char8 as BLC (ByteString, pack, concat, any, unpack, toStrict, append, lines, unlines)
import           System.IO     (appendFile)
import           Data.DateTime (DateTime, addSeconds, parseDateTime)
import           Data.Maybe    (fromJust)
import           Data.List     (intersperse, transpose, elemIndex, elem)
import           Data.List.Split (splitOn)
import           Data.Char       (isDigit, isSpace)
import           Control.Applicative ((<$>))
import qualified Data.Vector as DV
import qualified Data.Csv as CSV

constantSWMMEpoch :: DateTime
constantSWMMEpoch = fromJust $ parseDateTime "%Y-%m-%d %H:%M:%S" "1899-12-30 00:00:00"

daysToSeconds :: Num a => a -> a
daysToSeconds x = x * 24 * 60 * 60

getSWMMTime :: Double -> DateTime
getSWMMTime daysSinceSWMMEpoch = addSeconds secondsSinceSWMMEpoch constantSWMMEpoch
                                 where secondsSinceSWMMEpoch = round . daysToSeconds
                                                             $ daysSinceSWMMEpoch

parseFileInput :: FilePath -> FilePath
parseFileInput (' ' :xs) = parseFileInput xs
parseFileInput ('\'':xs) = parseFileInput xs
parseFileInput xs = go (reverse xs)
                    where go (' ' :ys) = go ys
                          go ('\'':ys) = go ys
                          go       ys  = reverse ys

parseUserOptions :: String -> Maybe [Int]
parseUserOptions s
    | s == "" || all isSpace s = Nothing
    | otherwise = Just $ concat $ map convertToUserSelections sList
                  where sList = splitOn "," s
                        convertToUserSelections s
                           | all isDigit s = [read s :: Int]
                           | otherwise           = splitConvert s
                                                   where splitConvert s = [a..b]
                                                         tl = splitOn "-" s
                                                         a  = read (tl !! 0) :: Int
                                                         b  = read (tl !! 1) :: Int

printZippedOptions :: (Int, String) -> IO ()
printZippedOptions (a, b) = print $ show a ++ " - " ++ b

getIndices :: [Int] -> [a] -> [a]
getIndices js xs = [a | a <- xs, (fromJust $ elemIndex a xs) `elem` js]

getUserVariables :: [Integer] -> [BLC.ByteString] -> IO [Int]
getUserVariables x y = do let options = map fromInteger x
                          mapM_ printZippedOptions (zip options (subcatchmentCodes y))
                          putStrLn "Please enter your choices: "
                          userChoices <- parseUserOptions <$> getLine
                          putStrLn ""
                          if   userChoices /= Nothing
                          then return $ fromJust userChoices
                          else return options

getUserIds :: [BLC.ByteString] -> IO [Int]
getUserIds names = do let options = map BLC.unpack names
                      mapM_ printZippedOptions (zip [0..] options)
                      putStrLn "Please enter your choices: "
                      userChoices <- parseUserOptions <$> getLine
                      putStrLn ""
                      if   userChoices /= Nothing
                      then return $ fromJust userChoices
                      else return $ take (length options) [0..]

subcatchmentCodes :: [BLC.ByteString] -> [String]
subcatchmentCodes pollutants = [ "Rainfall"
                               , "Snow Depth"
                               , "Evaporation Loss"
                               , "Infiltration Loss"
                               , "Runoff rate"
                               , "Groundwater Outflow Rate"
                               , "Groundwater Water Table Elevation"
                               , "Unsaturated zone moisture content"
                               ] ++ map BLC.unpack pollutants

nodeCodes :: [BLC.ByteString] -> [String]
nodeCodes pollutants = [ "Depth of water above invert"
                       , "Hydraulic Head"
                       , "Volume of stored + ponded water"
                       , "Lateral inflow"
                       , "Total inflow (lateral + upstream)"
                       , "Flow lost to flooding"
                       ] ++ map BLC.unpack pollutants

linkCodes :: [BLC.ByteString] -> [String]
linkCodes pollutants = [ "Flow rate"
                       , "Flow depth"
                       , "Flow velocity"
                       , "Flow volume"
                       , "Fraction of conduit's area filled or setting for non-conduits"
                       ] ++ map BLC.unpack pollutants

systemIdNames :: [BLC.ByteString]
systemIdNames = [ "Air temperature"
                , "Rainfall"
                , "Snow Depth"
                , "Evaporation + infiltration loss rate"
                , "Runoff flow"
                , "Dry weather inflow"
                , "Groundwater inflow"
                , "RDII inflow"
                , "User supplied direct inflow"
                , "Total lateral inflow (sum of variables 4 to 8)"
                , "Flow lost to flooding"
                , "Flow leaving through outfalls"
                , "Volume of stored water"
                , "Evaporation Rate"
                ]

printAllValues swmm fIds fVars strOption output fValue dateTimes = do
    let idsList = fIds . ids $ swmm
    let codeNumbers = (codeNumberVariables . fVars . variables) swmm
    putStrLn $ "Select " ++ strOption ++ " variables: "
    userVariables <- getUserVariables codeNumbers (pollutantIds . ids $ swmm)
    putStrLn $ "Select " ++ strOption ++ " ids: "
    userColumns <- getUserIds idsList
    mapM_ (\y -> do let outputFile = output ++ "/" ++ strOption ++ show y ++ ".csv"
                    let csvHeader = CSV.encode ["DateTime" : (map BLC.unpack (getIndices userColumns idsList))]
                    BL.appendFile outputFile csvHeader
                    let values =
                           (map ( (getIndices userColumns)
                                . (getValue fValue y)
                                ) (result swmm))
                    let csvValues = CSV.encode values
                    let csvWithDates = BLC.unlines (zipWith (zipDateTimeWithValues) dateTimes (BLC.lines csvValues))
                    BL.appendFile outputFile csvWithDates
          ) userVariables

getValue :: (a -> [[b]]) -> Int -> a -> [b]
getValue f y x = (transpose . f $ x) !! y

zipDateTimeWithValues a b = BLC.append (BLC.pack ((show (getSWMMTime a))++",")) b

main :: IO ()
main = do
    putStrLn "Please drag or drop the file, or enter filepath: "
    file <- getLine
    let input = parseFileInput file
    putStrLn ""
    lazy <- BL.readFile input
    let swmmObject = parseSWMMBinary lazy
    let dateTimes = map dateTimeValue $ result swmmObject
    putStrLn "Enter output file directory: "
    outFile <- getLine
    let output = parseFileInput outFile
    putStrLn ""

    -- All subcatchments
    printAllValues swmmObject subcatchmentIds subcatchmentVariables "subcatchment"
                   output subcatchmentValue dateTimes

    -- All nodes
    printAllValues swmmObject nodeIds nodeVariables "node"
                   output nodeValue dateTimes

    -- All links
    printAllValues swmmObject linkIds linkVariables "link"
                   output linkValue dateTimes

    -- All systems
    let systemIdsList = systemIdNames
    let outputFileName = output ++ "/system.csv"
    putStrLn "Select system ids: "
    userSystemColumns <- getUserIds systemIdsList
    let csvHeader = CSV.encode ["DateTime" : (map BLC.unpack (getIndices userSystemColumns systemIdsList))]
    BL.appendFile outputFileName csvHeader
    let values = map ((getIndices userSystemColumns) . systemValue) (result swmmObject)
    let csvValues = CSV.encode values
    let csvWithDates = BLC.unlines (zipWith (zipDateTimeWithValues) dateTimes (BLC.lines csvValues))
    BL.appendFile outputFileName csvWithDates

    print "Done"

