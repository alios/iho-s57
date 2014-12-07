{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.FRID where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

data FOID =
  FOID { _foidProducingAgency :: ! Int
       , _foidIdentificationNumber :: ! Int
       , _foidFeatureIdentificationSub :: ! Int
       } deriving (Show, Eq, Data, Typeable)

makeClassy ''FOID

instance FromS57FileRecord FOID where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "FOID") = error $ "not an FOID record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "FOID: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
        in FOID { _foidProducingAgency = lookupField "AGEN"
                , _foidIdentificationNumber = lookupField "FIDN"
                , _foidFeatureIdentificationSub = lookupField "FIDS"
                }

mkAttrs :: S57FileRecord -> Map Int Text
mkAttrs r =
  let rv = structureMultiField . rootLabel $ r
      lookupFieldM k r =
        maybe (error $ "mkAttrs: unable to lookup key " ++ T.unpack k)
        id $ Map.lookup k r
      lookupField k r = fromS57Value $ lookupFieldM k r
      mkATTF r = (lookupField "*ATTL" r, lookupField "ATVL" r)
  in Map.fromList $ fmap mkATTF rv




data FFPC =
  FFPC { _ffpcUpdateInstruction :: ! UpdateInstruction
       , _ffpcObjectPointerIndex :: ! Int
       , _ffpcFeatureObjectPointers :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''FFPC

instance FromS57FileRecord FFPC where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "FFPC") = error $ "not an FFPC record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "FFPC: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
        in FFPC { _ffpcUpdateInstruction = lookupField "FFUI"
                , _ffpcObjectPointerIndex = lookupField "FFIX"
                , _ffpcFeatureObjectPointers = lookupField "NFPT"
                }

data GeometricPrimitive = Point | Line | Area | NoRef
  deriving (Show, Eq, Data, Typeable)

instance Enum GeometricPrimitive where
  toEnum 1 = Point
  toEnum 2 = Line
  toEnum 3 = Area
  toEnum 255 = NoRef
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a GeometricPrimitive"
  fromEnum Point = 1
  fromEnum Line = 2
  fromEnum Area = 3
  fromEnum NoRef = 255

instance FromS57Value GeometricPrimitive where
  fromS57Value (S57CharData "P") = Point
  fromS57Value (S57CharData "L") = Line
  fromS57Value (S57CharData "A") = Area
  fromS57Value (S57CharData "N") = NoRef
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value GeometricPrimitive undefined for " ++ show v

data FRID =
  FRID { _fridRecordName :: ! RecordName
       , _fridGeometricPrimtive :: ! GeometricPrimitive
       , _fridGroup :: ! (Maybe Int)
       , _fridObjectLabel :: ! Int
       , _fridVersion :: ! Int
       , _fridUpdateInstruction :: ! UpdateInstruction
       , _fridFOID :: ! FOID
       , _fridATTFs :: ! (Map Int Text)
       , _fridNATFs :: ! (Map Int Text)
       , _fridFFPC :: ! (Maybe FFPC)
       }  deriving (Show, Eq, Data, Typeable)
makeLenses ''FRID

instance HasRecordName FRID where
  recordName = fridRecordName

instance HasFOID FRID where
  fOID = fridFOID
  
instance FromS57FileRecord FRID where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "FRID") = error $ "not an FRID record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "FRID: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
            rn = RecordName { _rcnm = lookupField "RCNM", _rcid = lookupField "RCID" }
            groupV = lookupField "GRUP"
        in   FRID { _fridRecordName = rn
                  , _fridGeometricPrimtive = lookupField "PRIM"
                  , _fridGroup = if ((groupV >= 1) && (groupV <=254))
                                 then Just groupV else Nothing
                  , _fridObjectLabel = lookupField "OBJL"
                  , _fridVersion = lookupField "RVER"
                  , _fridUpdateInstruction = lookupField "RUIN"
                  , _fridFOID = fromS57FileRecord $ lookupChildField "FRID" r "FOID"
                  , _fridATTFs = maybe mempty mkAttrs $ lookupChildFieldM "FRID" r "ATTF"
                  , _fridNATFs = maybe mempty mkAttrs $ lookupChildFieldM "FRID" r "NATF"
                  , _fridFFPC = fmap fromS57FileRecord $ lookupChildFieldM "FRID" r "FFPC"
                  }
