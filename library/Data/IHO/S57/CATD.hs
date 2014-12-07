{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.CATD where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types

             

data CATD =
  CATD { _catdRecordName :: ! RecordName
       , _catdFileName :: ! Text
       , _catdFileLongName :: ! Text
       , _catdVolume :: ! Text
       , _catdImplementation :: ! Text
       , _catdSouthernMostLatitude :: ! Double
       , _catdWesternMostLongitude :: ! Double
       , _catdNothernMostLatitude :: ! Double
       , _catdEasternMostLongitude :: ! Double
       , _catdCrc :: ! Text
       , _catdComment :: ! Text    
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''CATD

instance HasRecordName CATD where
  recordName = catdRecordName


instance FromS57FileRecord CATD where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "CATD") = error $ "not an CATD record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "CATD: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
            rcnmV = case (lookupFieldM "RCNM") of
                     (S57CharData v) -> readRecordNameT v
                     (S57Int v) -> toEnum v
                     v -> error $ "invalid RCNM value: " ++ show v
            rn = RecordName { _rcnm = rcnmV, _rcid = lookupField "RCID" }
        in CATD { _catdRecordName = rn
                , _catdFileName = lookupField "FILE"
                , _catdFileLongName = lookupField "LFIL"
                , _catdVolume = lookupField "VOLM"
                , _catdImplementation = lookupField "IMPL"
                , _catdSouthernMostLatitude = lookupField "SLAT"
                , _catdWesternMostLongitude = lookupField "WLON"
                , _catdNothernMostLatitude = lookupField "NLAT"
                , _catdEasternMostLongitude = lookupField "ELON"
                , _catdCrc = lookupField "CRCS"
                , _catdComment = lookupField "COMT"
          }
