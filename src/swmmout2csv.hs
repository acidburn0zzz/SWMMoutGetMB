-- |
-- Module : Water.SWMM.SWMMout2csv
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

import           SWMM
import qualified Data.ByteString.Lazy as BL        (appendFile, ByteString, readFile)
import qualified Data.ByteString.Lazy.Char8 as BLC (pack, concat)
import           System.IO     (appendFile)
import           Data.DateTime (DateTime(..), addSeconds, parseDateTime)
import           Data.Maybe    (fromJust)
import           Data.List     (intersperse, transpose)

constantSWMMEpoch :: DateTime
constantSWMMEpoch = fromJust $ parseDateTime "%Y-%m-%d %H:%M:%S" "1899-12-30 00:00:00"

daysToSeconds :: Num a => a -> a
daysToSeconds x = x * 24 * 60 * 60

getSWMMTime :: Double -> DateTime
getSWMMTime daysSinceSWMMEpoch = addSeconds (round (daysToSeconds daysSinceSWMMEpoch)) constantSWMMEpoch

parseFileInput :: FilePath -> FilePath
parseFileInput (' ' :xs) = parseFileInput xs
parseFileInput ('\'':xs) = parseFileInput xs
parseFileInput xs = go (reverse xs)
                    where go (' ' :ys) = go ys
                          go ('\'':ys) = go ys
                          go       ys  = reverse ys

addQuotes list = map (\x -> BLC.concat ["\"", x, "\""]) list

printHeader :: FilePath -> [BL.ByteString] -> IO ()
printHeader output list = do let bsList = "\"DateTime\"" : addQuotes list
                             let csvList = intersperse "," bsList
                             mapM_ (BL.appendFile output) csvList
                             appendFile output "\n"

printHeaderInteger :: FilePath -> [Integer] -> IO ()
printHeaderInteger output list = do let strList = "DateTime" : map show list
                                    let csvList = intersperse "," strList
                                    mapM_ (appendFile output) csvList
                                    appendFile output "\n"

printSingleLine :: FilePath -> (Double, [Float]) -> IO ()
printSingleLine output (a, b) = do let list = show (getSWMMTime a) : map show b
                                   let csvList = intersperse "," list
                                   mapM_ (appendFile output) csvList
                                   appendFile output "\n"

main :: IO ()
main = do
    putStrLn "Please drag or drop the file, or enter filepath: "
    file <- getLine
    let input = parseFileInput file
    print input
    lazy <- BL.readFile input
    let swmmObject = parseSWMMBinary lazy
    let dateTimes = map dateTimeValue $ result swmmObject
    putStrLn "Enter output file directory: "
    outFile <- getLine
    let output = parseFileInput outFile
    -- All subcatchments
    let subcatchmentIdsList = subcatchmentIds . ids $ swmmObject
    mapM_ (\y -> do let outputFileName = output ++ "/subcatchment" ++ show y ++ ".csv"
                    printHeader outputFileName subcatchmentIdsList
                    let subcatchmentValuesWithDateTime =
                            zip dateTimes (map (\x -> (transpose . subcatchmentValue $ x) !! y) $ result swmmObject)
                    mapM_ (printSingleLine outputFileName) subcatchmentValuesWithDateTime
          ) (map fromInteger ((codeNumberVariables . subcatchmentVariables . variables) swmmObject))
    -- All nodes
    let nodeIdsList = nodeIds . ids $ swmmObject
    mapM_ (\y -> do let outputFileName = output ++ "/node" ++ show y ++ ".csv"
                    printHeader outputFileName nodeIdsList
                    let nodeValuesWithDateTime =
                            zip dateTimes (map (\x -> (transpose . nodeValue $ x) !! y) $ result swmmObject)
                    mapM_ (printSingleLine outputFileName) nodeValuesWithDateTime
          ) (map fromInteger ((codeNumberVariables . nodeVariables . variables) swmmObject))
    -- All links
    let linkIdsList = linkIds . ids $ swmmObject
    mapM_ (\y -> do let outputFileName = output ++ "/link" ++ show y ++ ".csv"
                    printHeader outputFileName linkIdsList
                    let linkValuesWithDateTime =
                            zip dateTimes (map (\x -> (transpose . linkValue $ x) !! y) $ result swmmObject)
                    mapM_ (printSingleLine outputFileName) linkValuesWithDateTime
          ) (map fromInteger ((codeNumberVariables . linkVariables . variables) swmmObject))
    -- All systems
    let systemIdsList = codeNumberVariables . systemVariables . variables $ swmmObject
    let outputFileName = output ++ "/system" ++ ".csv"
    printHeaderInteger outputFileName systemIdsList
    let systemValuesWithDateTime = zip dateTimes (map systemValue $ result swmmObject)
    mapM_ (printSingleLine outputFileName) systemValuesWithDateTime
    print "Done"

