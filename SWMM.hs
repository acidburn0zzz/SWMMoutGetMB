-- |
-- Module : Water.SWMM
-- Copyright : (C) 2014 Siddhanathan Shanmugam
-- License : LGPL (see LICENSE)
-- Maintainer : siddhanathan@gmail.com
-- Portability : very
-- 
-- Parser for SWMM 5 Binary .OUT files
--

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module SWMM ( SWMMObject(..)
            , Header(..)
            , Ids(..)
            , Properties(..)
            , ObjectProperties(..)
            , Variables(..)
            , ReportingVariables(..)
            , ReportingInterval(..)
            , ValuesForOneDateTime(..)
            , ComputedResult
            , ClosingRecord(..)
            , parseSWMMBinary
            ) where

import           Data.Binary.Get            (getWord32le, runGetState, Get(..), getByteString)
import           Data.Word                  (Word32(..))
import           Data.ByteString.Internal   (ByteString)
import           Control.Applicative        ((<$>), (<*>))
import           Data.Binary.IEEE754        (getFloat32le, getFloat64le)
import qualified Data.ByteString.Lazy as BL (ByteString, pack, unpack)

data SWMMObject = SWMMObject { header        :: Header
                             , ids           :: Ids
                             , properties    :: ObjectProperties
                             , variables     :: ReportingVariables
                             , intervals     :: ReportingInterval
                             , result        :: ComputedResult
                             , closingRecord :: ClosingRecord
                             } deriving (Show)

data Header = Header { headerIdNumber        :: Integer
                     , versionNumber         :: Integer
                     , codeNumber            :: Integer
                     , numberOfSubcatchments :: Integer
                     , numberOfNodes         :: Integer
                     , numberOfLinks         :: Integer
                     , numberOfPollutants    :: Integer
                     } deriving (Show)

data Ids = Ids { subcatchmentIds  :: [ByteString]
               , nodeIds          :: [ByteString]
               , linkIds          :: [ByteString]
               , pollutantIds     :: [ByteString]
               , concentrationIds :: [Integer]
               } deriving (Show)

data ObjectProperties = ObjectProperties { subcatchmentProperties :: Properties
                                         , nodeProperties         :: Properties
                                         , linkProperties         :: Properties
					 } deriving (Show)

data ReportingVariables = ReportingVariables { subcatchmentVariables :: Variables
                                             , nodeVariables         :: Variables
                                             , linkVariables         :: Variables
                                             , systemVariables       :: Variables
                                             } deriving (Show)


data ReportingInterval = ReportingInterval { startDateTime :: Double
                                           , timeIntervals :: Integer
                                           } deriving (Show)

type ComputedResult = [ValuesForOneDateTime]

data ClosingRecord = ClosingRecord { idBytePosition         :: Integer
                                   , propertiesBytePosition :: Integer
                                   , resultBytePosition     :: Integer
                                   , numberOfPeriods        :: Integer
                                   , errorCode              :: Integer
                                   , closingIdNumber        :: Integer
                                   } deriving (Show)

data Properties = Properties { numberOfProperties   :: Integer
                             , codeNumberProperties :: [Integer]
                             , valueProperties      :: [Float]
                             } deriving (Show)

data Variables = Variables { numberOfVariables   :: Integer
                           , codeNumberVariables :: [Integer]
                           } deriving (Show)

data ValuesForOneDateTime = ValuesForOneDateTime { dateTimeValue     :: Double
                                                 , subcatchmentValue :: [Float]
                                                 , nodeValue         :: [Float]
                                                 , linkValue         :: [Float]
                                                 , systemValue       :: [Float]
                                                 } deriving (Show)

closingRecordSize :: Int
closingRecordSize = 6 * 4

getHeader :: Get Header
getHeader = Header <$> a
                   <*> a
                   <*> a
                   <*> a
                   <*> a
                   <*> a
                   <*> a
            where a = getIntegerWord32le

parseSWMMBinary :: BL.ByteString -> SWMMObject
parseSWMMBinary input = do
    let closingByteString  = getClosingByteString input
        (closingRecord, _) = getClosingRecords closingByteString
        (header, rest1, _) = runGetState getHeader input 1
        (ids, rest2) = getIds rest1 header
        (objectProperties, rest3) = getObjectProperties header rest2
        (reportingVariables, rest4) = getReportingVariables rest3
        (reportingIntervals, rest5) = getReportingIntervals rest4
        (result, rest6) = getComputedResults (numberOfPeriods closingRecord)
                                             reportingVariables header rest5
    SWMMObject header
               ids
               objectProperties
               reportingVariables
               reportingIntervals
               result
               closingRecord

getIntegerWord32le :: Get Integer
getIntegerWord32le = fromIntegral <$> getWord32le

getWords :: Integer -> BL.ByteString -> ([Integer], BL.ByteString)
getWords n input
    | n == 0    = ([], input)
    | otherwise = appendW w (getWords (n-1) rest)
                  where (w, rest, _) = runGetState getIntegerWord32le input 1

appendW :: a -> ([a], BL.ByteString) -> ([a], BL.ByteString)
appendW w (x, y) = (w:x, y)

getDecimals :: Integer -> BL.ByteString -> ([Float], BL.ByteString)
getDecimals n input
    | n == 0    = ([], input)
    | otherwise = appendW w (getDecimals (n-1) rest)
                  where (w, rest, _) = runGetState getFloat32le input 1

getByteStrings :: Integer -> BL.ByteString -> ([ByteString], BL.ByteString)
getByteStrings n input
    | n == 0    = ([], input)
    | otherwise = appendW w (getByteStrings (n-1) rest)
                  where (c, r, _) = runGetState getWord32le input 1
                        (w, rest, _) = runGetState ((getByteString . fromIntegral) c) r 1

getClosingByteString :: BL.ByteString -> BL.ByteString
getClosingByteString = BL.pack . reverse . take closingRecordSize . reverse . BL.unpack

getIds :: BL.ByteString -> Header -> (Ids, BL.ByteString)
getIds rest1 header = do
    let (subcatchments, rest2) = getByteStrings (numberOfSubcatchments header) rest1
        (nodes, rest3)         = getByteStrings (numberOfNodes header) rest2
        (links, rest4)         = getByteStrings (numberOfLinks header) rest3
        (pollutants, rest5)    = getByteStrings (numberOfPollutants header) rest4
        (pollutantConcentrationUnits, rest6) = getWords (numberOfPollutants header) rest5
    (Ids subcatchments nodes links pollutants pollutantConcentrationUnits, rest6)

getObjectProperties :: Header -> BL.ByteString -> (ObjectProperties, BL.ByteString)
getObjectProperties header rest1 = do
    let (numberOfSubcatchmentProperties, rest2, _) = runGetState getIntegerWord32le rest1 1
        (codeNumberSubcatchmentProperties, rest3)  = getWords numberOfSubcatchmentProperties rest2
        (valueSubcatchmentProperties, rest4)       = getDecimals n rest3
                                                     where n = numberOfSubcatchmentProperties
                                                             * numberOfSubcatchments header
        (numberOfNodeProperties, rest5, _)         = runGetState getIntegerWord32le rest4 1
        (codeNumberNodeProperties, rest6)          = getWords numberOfNodeProperties rest5
        (valueNodeProperties, rest7)               = getDecimals (numberOfNodeProperties
                                                                 * numberOfNodes header) rest6
        (numberOfLinkProperties, rest8, _)         = runGetState getIntegerWord32le rest7 1
        (codeNumberLinkProperties, rest9)          = getWords numberOfLinkProperties rest8
        (valueLinkProperties, rest10)              = getDecimals (numberOfLinkProperties
                                                                 * numberOfLinks header) rest9
        subcatchment = Properties numberOfSubcatchmentProperties
                                  codeNumberSubcatchmentProperties
                                  valueSubcatchmentProperties
        node         = Properties numberOfNodeProperties
                                  codeNumberNodeProperties
                                  valueNodeProperties
        link         = Properties numberOfLinkProperties
                                  codeNumberLinkProperties
                                  valueLinkProperties
        object = ObjectProperties subcatchment node link
    (object, rest10)

getReportingVariables :: BL.ByteString -> (ReportingVariables, BL.ByteString)
getReportingVariables rest1 = do
    let (numberOfSubcatchmentVariables, rest2, _) = runGetState getIntegerWord32le rest1 1
        (codeNumberSubcatchmentVariables, rest3)  = getWords numberOfSubcatchmentVariables rest2
        (numberOfNodeVariables, rest4, _)         = runGetState getIntegerWord32le rest3 1
        (codeNumberNodeVariables, rest5)          = getWords numberOfNodeVariables rest4
        (numberOfLinkVariables, rest6, _)         = runGetState getIntegerWord32le rest5 1
        (codeNumberLinkVariables, rest7)          = getWords numberOfLinkVariables rest6
        (numberOfSystemVariables, rest8, _)       = runGetState getIntegerWord32le rest7 1
        (codeNumberSystemVariables, rest9)        = getWords numberOfSystemVariables rest8
        subcatchment = Variables numberOfSubcatchmentVariables codeNumberSubcatchmentVariables
        node         = Variables numberOfNodeVariables codeNumberNodeVariables
        link         = Variables numberOfLinkVariables codeNumberLinkVariables
        system       = Variables numberOfSystemVariables codeNumberSystemVariables
        reportingVariables = ReportingVariables subcatchment node link system
    (reportingVariables, rest9)

getReportingIntervals :: BL.ByteString -> (ReportingInterval, BL.ByteString)
getReportingIntervals rest1 = do
    let (startDateTime, rest2, _) = runGetState getFloat64le rest1 1
        (timeIntervals, rest3, _) = runGetState getIntegerWord32le rest2 1
        reportingInterval = ReportingInterval startDateTime timeIntervals
    (reportingInterval, rest3)

getComputedResults :: Integer -> ReportingVariables -> Header -> BL.ByteString
                   -> ([ValuesForOneDateTime], BL.ByteString)
getComputedResults n reportingVariables header rest1
    | n == 0    = ([], rest1)
    | otherwise = do
        let (dateTimeValue, rest2, _)  = runGetState getFloat64le rest1 1
            (subcatchmentValue, rest3) = getDecimals n rest2
                where n = numberOfSubcatchments header
                        * (numberOfVariables . subcatchmentVariables) reportingVariables
            (nodeValue, rest4) = getDecimals n rest3
                where n = numberOfNodes header
                        * (numberOfVariables . nodeVariables) reportingVariables
            (linkValue, rest5) = getDecimals n rest4
                where n = numberOfLinks header
                        * (numberOfVariables . linkVariables) reportingVariables
            (systemValue, rest6) = getDecimals n rest5
                where n = (numberOfVariables . systemVariables) reportingVariables
            valueConstructor = ValuesForOneDateTime dateTimeValue
                                                    subcatchmentValue
                                                    nodeValue
                                                    linkValue
                                                    systemValue
        appendW valueConstructor (getComputedResults (n-1) reportingVariables header rest6)

getClosingRecords :: BL.ByteString -> (ClosingRecord, BL.ByteString)
getClosingRecords closingByteString = do
    let (objectIdBytePosition, rest2, _) = runGetState getIntegerWord32le closingByteString 1
        (objectPropertiesBytePosition, rest3, _) = runGetState getIntegerWord32le rest2 1
        (computedResultsBytePosition, rest4, _)  = runGetState getIntegerWord32le rest3 1
        (numberOfReportingPeriods, rest5, _)     = runGetState getIntegerWord32le rest4 1
        (errorCodeStatus, rest6, _)              = runGetState getIntegerWord32le rest5 1
        (closingIdNumber, rest7, _)              = runGetState getIntegerWord32le rest6 1
    (ClosingRecord objectIdBytePosition
                   objectPropertiesBytePosition
                   computedResultsBytePosition
                   numberOfReportingPeriods
                   errorCodeStatus
                   closingIdNumber,               rest7)

