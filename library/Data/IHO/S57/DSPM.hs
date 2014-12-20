{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.DSPM where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable, cast)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types


data CoordinateUnits =
  LatitudeLongitude |
  EastingNorthing |
  UnitsOfMap
  deriving (Show, Eq, Data, Typeable)

instance Enum CoordinateUnits where
  toEnum 1 = LatitudeLongitude
  toEnum 2 = EastingNorthing
  toEnum 3 = UnitsOfMap
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a CoordinateUnits"
  fromEnum LatitudeLongitude = 1
  fromEnum EastingNorthing = 2
  fromEnum UnitsOfMap = 3

instance FromS57Value CoordinateUnits where
  fromS57Value (S57CharData "LL") = LatitudeLongitude
  fromS57Value (S57CharData "EN") = EastingNorthing
  fromS57Value (S57CharData "UM") = UnitsOfMap
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value CoordinateUnits undefined for " ++
                   show v

data DSPM =
  DSPM { _dspmHorizontalDatum :: ! Int
       , _dspmVerticalDatum :: ! Int
       , _dspmSoundingDatum :: ! Int
       , _dspmCompilationScale :: !Int
       , _dspmUnitsOfDepth :: ! Int
       , _dspmUnitsOfHeight :: ! Int
       , _dspmUnitsOfPosAccuracy :: ! Int
       , _dspmCoordinateUnits :: ! CoordinateUnits
       , _dspmCoordinateMulFactor :: ! Int
       , _dspmSoundingMulFactor :: ! Int
       , _dspmComment :: ! Text
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''DSPM



instance FromS57FileRecord DSPM where
  fromS57FileDataRecord r
    | ((structureFieldName . rootLabel $ r) /= "DSPM") =
        error $ "not an DSPM record: " ++ show r
    | otherwise =
        DSPM { _dspmHorizontalDatum = lookupField r "HDAT"
             , _dspmVerticalDatum = lookupField r "VDAT" 
             , _dspmSoundingDatum = lookupField r "SDAT"
             , _dspmCompilationScale = lookupField r "CSCL"
             , _dspmUnitsOfDepth = lookupField r "DUNI"
             , _dspmUnitsOfHeight = lookupField r "HUNI"
             , _dspmUnitsOfPosAccuracy = lookupField r "PUNI"
             , _dspmCoordinateUnits = lookupField r "COUN"
             , _dspmCoordinateMulFactor = lookupField r "COMF"
             , _dspmSoundingMulFactor = lookupField r "SOMF"
             , _dspmComment = lookupField r "COMT"
             }

    
