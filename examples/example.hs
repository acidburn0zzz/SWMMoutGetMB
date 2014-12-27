-- |
-- Module : Examples.example
-- Copyright : (C) 2014 Siddhanathan Shanmugam
-- License : LGPL (see LICENSE)
-- Maintainer : siddhanathan@gmail.com
-- Portability : very
--
-- Example of a program using SWMMoutGetMB for parsing SWMM .OUT files
--

module Main (main) where

import           Water.SWMM                 (parseSWMMBinary)
import qualified Data.ByteString.Lazy as BL (readFile)

parseFileInput :: String -> String
parseFileInput (' ' :xs) = parseFileInput xs
parseFileInput ('\'':xs) = parseFileInput xs
parseFileInput xs = go (reverse xs)
                    where go (' ' :ys) = go ys
                          go ('\'':ys) = go ys
                          go       ys  = reverse ys

main :: IO ()
main = do
    putStrLn "Please drag or drop the file, or enter filepath: "
    file <- getLine
    input <- BL.readFile $ parseFileInput file
    let swmmObject = parseSWMMBinary input
    print swmmObject

