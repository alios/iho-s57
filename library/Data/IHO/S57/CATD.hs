{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.CATD where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)

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
  fromS57FileRecord r = undefined
