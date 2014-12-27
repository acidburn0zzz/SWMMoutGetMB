module Main (main) where

import qualified SWMM as SWMM
import qualified Data.ByteString.Lazy as BL (readFile)
import           System.IO

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
    let swmmObject = SWMM.parseSWMMBinary input
    print swmmObject

