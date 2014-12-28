-- |
-- Module : Water.SWMM
-- Copyright : (C) 2014 Siddhanathan Shanmugam
-- License : LGPL (see LICENSE)
-- Maintainer : siddhanathan@gmail.com
-- Portability : very
--
-- Parser for SWMM 5 Binary .OUT files
--

module Water.SWMM ( SWMMObject(..)
                  , Header(..)
                  , ObjectIds(..)
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

import           Data.Binary.Get            (getWord32le, runGet, Get, getLazyByteString)
import           Control.Applicative        ((<$>), (<*>))
import           Data.Binary.IEEE754        (getFloat32le, getFloat64le)
import qualified Data.ByteString.Lazy as BL (ByteString, pack, unpack)
import           Data.List.Split            (chunksOf)
import           Control.Monad              (replicateM)

data SWMMObject = SWMMObject { header        :: Header
                             , ids           :: ObjectIds
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

data ObjectIds = ObjectIds { subcatchmentIds  :: [BL.ByteString]
                           , nodeIds          :: [BL.ByteString]
                           , linkIds          :: [BL.ByteString]
                           , pollutantIds     :: [BL.ByteString]
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

type Ids = [BL.ByteString]

data Properties = Properties { numberOfProperties   :: Integer
                             , codeNumberProperties :: [Integer]
                             , valueProperties      :: [Float]
                             } deriving (Show)

data Variables = Variables { numberOfVariables   :: Integer
                           , codeNumberVariables :: [Integer]
                           } deriving (Show)

data ValuesForOneDateTime = ValuesForOneDateTime { dateTimeValue     :: Double
                                                 , subcatchmentValue :: [[Float]]
                                                 , nodeValue         :: [[Float]]
                                                 , linkValue         :: [[Float]]
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

getSWMMObject :: ClosingRecord -> Get SWMMObject
getSWMMObject closing = do
    header <- getHeader
    objectIds <- getObjectIds header
    objectProperties <- getObjectProperties header
    reportingVariables <- getReportingVariables
    reportingIntervals <- getReportingIntervals
    computedResult <- getComputedResults (numberOfPeriods closing) header reportingVariables
    closingRecord <- getClosingRecords
    return $ SWMMObject header
                        objectIds
                        objectProperties
                        reportingVariables
                        reportingIntervals
                        computedResult
                        closingRecord

parseSWMMBinary :: BL.ByteString -> SWMMObject
parseSWMMBinary input = do
    let closingRecord = runGet getClosingRecords (getClosingByteString input)
        swmmObject    = runGet (getSWMMObject closingRecord) input
    swmmObject

getIntegerWord32le :: Get Integer
getIntegerWord32le = fromIntegral <$> getWord32le

getClosingByteString :: BL.ByteString -> BL.ByteString
getClosingByteString = BL.pack . reverse . take closingRecordSize . reverse . BL.unpack

getObjectIds :: Header -> Get ObjectIds
getObjectIds header =
    ObjectIds <$> getIds (numberOfSubcatchments header)
              <*> getIds (numberOfNodes header)
              <*> getIds (numberOfLinks header)
              <*> getIds (numberOfPollutants header)
              <*> replicateM (fromInteger . numberOfPollutants $ header) getIntegerWord32le

getIds :: Integer -> Get [BL.ByteString]
getIds n = replicateM (fromInteger n)
                      (getIntegerWord32le >>= getLazyByteString . fromInteger)

getVariables :: Get Variables
getVariables = do
    number <- getIntegerWord32le
    codeNumbers <- replicateM (fromInteger number) getIntegerWord32le
    return $ Variables number codeNumbers

getProperties :: Integer -> Get Properties
getProperties n = do
    number <- getIntegerWord32le
    codeNumbers <- replicateM (fromInteger number) getIntegerWord32le
    values <- replicateM (fromInteger (number * n)) getFloat32le
    return $ Properties number codeNumbers values

getObjectProperties :: Header -> Get ObjectProperties
getObjectProperties header = ObjectProperties <$> getProperties (numberOfSubcatchments header)
                                              <*> getProperties (numberOfNodes header)
                                              <*> getProperties (numberOfLinks header)

getReportingVariables :: Get ReportingVariables
getReportingVariables = ReportingVariables <$> a
                                           <*> a
                                           <*> a
                                           <*> a
                        where a = getVariables

getReportingIntervals :: Get ReportingInterval
getReportingIntervals = ReportingInterval <$> getFloat64le
                                          <*> getIntegerWord32le

getResults :: Header -> ReportingVariables -> Get ValuesForOneDateTime
getResults header report = do
    ValuesForOneDateTime <$> getFloat64le
                         <*> getSplitValues (numberOfSubcatchments header * ns) ns
                         <*> getSplitValues (numberOfNodes header * nn) nn
                         <*> getSplitValues (numberOfLinks header * nl) nl
                         <*> getValues (numberOfVariables . systemVariables $ report)
    where ns = (numberOfVariables . subcatchmentVariables) report
          nn = (numberOfVariables . nodeVariables) report
          nl = (numberOfVariables . linkVariables) report

getComputedResults :: Integer -> Header -> ReportingVariables -> Get ComputedResult
getComputedResults n header report = replicateM (fromInteger n) (getResults header report)

getValues :: Integer -> Get [Float]
getValues n = replicateM (fromInteger n) getFloat32le

getSplitValues :: Integer -> Integer -> Get [[Float]]
getSplitValues n nv = chunksOf (fromInteger nv) <$> replicateM (fromInteger n) getFloat32le

getClosingRecords :: Get ClosingRecord
getClosingRecords = ClosingRecord <$> a
                                  <*> a
                                  <*> a
                                  <*> a
                                  <*> a
                                  <*> a
                    where a = getIntegerWord32le

