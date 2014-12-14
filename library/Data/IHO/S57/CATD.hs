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
  CATD { _catdFileName :: ! Text
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

instance FromS57FileRecord CATD where
  fromS57FileDataRecord r
    | ((structureFieldName . rootLabel $ r) /= "CATD") =
        error $ "not an CATD record: " ++ show r
    | otherwise =
        CATD { _catdFileName = lookupField r "FILE"
             , _catdFileLongName = lookupField r "LFIL"
             , _catdVolume = lookupField r "VOLM"
             , _catdImplementation = lookupField r "IMPL"
             , _catdSouthernMostLatitude = lookupField r "SLAT"
             , _catdWesternMostLongitude = lookupField r "WLON"
             , _catdNothernMostLatitude = lookupField r "NLAT"
             , _catdEasternMostLongitude = lookupField r "ELON"
             , _catdCrc = lookupField r "CRCS"
             , _catdComment = lookupField r "COMT"
             }
