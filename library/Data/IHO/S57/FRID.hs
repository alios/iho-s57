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
import Data.ByteString (ByteString)

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
      lookupFieldM k _r =
        maybe (error $ "mkAttrs: unable to lookup key " ++ T.unpack k)
        id $ Map.lookup k _r
      lookupField k _r = fromS57Value $ lookupFieldM k _r
      mkATTF _r = (lookupField "*ATTL" _r, lookupField "ATVL" _r)
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


data RelationShipIndicator =
  Master | Slave | Peer
  deriving (Show, Eq, Data, Typeable)


instance Enum RelationShipIndicator where
  toEnum 1 = Master
  toEnum 2 = Slave
  toEnum 3 = Peer
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a RelationShipIndicator"
  fromEnum Master = 1
  fromEnum Slave = 2
  fromEnum Peer = 3

instance FromS57Value RelationShipIndicator where
  fromS57Value (S57CharData "M") = Master
  fromS57Value (S57CharData "S") = Slave
  fromS57Value (S57CharData "P") = Peer
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value RelationShipIndicator undefined for " ++ show v



data FFPT =
  FFPT { _ffptLongName :: ! ByteString
       , _ffptRelationShipIndicator :: ! RelationShipIndicator
       , _ffptComment :: ! Text
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''FFPT

mkFFPTs :: S57FileRecord -> [FFPT]
mkFFPTs r 
  | ((structureFieldName . rootLabel $ r) /= "FFPT") = error $ "not an FFPT record: " ++ show r
  | otherwise = 
      let rv = structureMultiField . rootLabel $ r
          lookupFieldM k _r =
            maybe (error $ "mkFFPTs: unable to lookup key " ++ T.unpack k)
            id $ Map.lookup k _r
          lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkFFPT _r = FFPT { _ffptLongName = lookupField "*LNAM" _r
                           , _ffptRelationShipIndicator = lookupField "RIND" _r
                           , _ffptComment = lookupField "COMT" _r
                           }
      in fmap mkFFPT rv


data FSPC =
  FSPC { _fspcSpatialPointerUpdateInstruction :: ! UpdateInstruction
       , _fspcSpatialPointerIndex :: ! Int
       , _fspcSpatialPointers :: ! Int
       } deriving (Show, Eq, Data, Typeable)

makeClassy ''FSPC

instance FromS57FileRecord FSPC where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "FSPC") = error $ "not an FSPC record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "FSPC: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
        in FSPC { _fspcSpatialPointerUpdateInstruction = lookupField "FSUI"
                , _fspcSpatialPointerIndex = lookupField "FSIX"
                , _fspcSpatialPointers = lookupField "NSPT"
                }



data Orientation
  = Forward | Reverse | NullOrientation
  deriving (Show, Eq, Data, Typeable)

instance Enum Orientation where
  toEnum 1 = Forward
  toEnum 2 = Reverse
  toEnum 255 = NullOrientation
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a Orientation"
  fromEnum Forward = 1
  fromEnum Reverse = 2
  fromEnum NullOrientation = 255

instance FromS57Value Orientation where
  fromS57Value (S57CharData "F") = Forward
  fromS57Value (S57CharData "R") = Reverse
  fromS57Value (S57CharData "N") = NullOrientation
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value Orientation undefined for " ++ show v


data UsageIndicator
  = Exterior | Interior | TruncatedExterior | NullUsage
  deriving (Show, Eq, Data, Typeable)

instance Enum UsageIndicator where
  toEnum 1 = Exterior
  toEnum 2 = Interior
  toEnum 3 = TruncatedExterior
  toEnum 255 = NullUsage
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a UsageIndicator"
  fromEnum Exterior = 1
  fromEnum Interior = 2
  fromEnum TruncatedExterior = 3
  fromEnum NullUsage = 255

instance FromS57Value UsageIndicator where
  fromS57Value (S57CharData "E") = Exterior
  fromS57Value (S57CharData "I") = Interior
  fromS57Value (S57CharData "C") = TruncatedExterior 
  fromS57Value (S57CharData "N") = NullUsage
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value UsageIndicator undefined for " ++ show v


data MaskingIndicator
  = Mask | Show | NullMask           
  deriving (Show, Eq, Data, Typeable)

instance Enum MaskingIndicator where
  toEnum 1 = Mask
  toEnum 2 = Show
  toEnum 255 = NullMask
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a MaskingIndicator"
  fromEnum Mask = 1
  fromEnum Show = 2
  fromEnum NullMask = 255


instance FromS57Value MaskingIndicator where
  fromS57Value (S57CharData "M") = Mask
  fromS57Value (S57CharData "S") = Show
  fromS57Value (S57CharData "N") = NullMask
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value MaskingIndicator undefined for " ++ show v


data FSPT =
  FSPT { _fsptName :: ! ByteString
       , _fsptOrientation :: ! Orientation
       , _fsptUsageIndicator :: ! UsageIndicator
       , _fsptMaskingIndicator :: ! MaskingIndicator
       } deriving (Show, Eq, Data, Typeable)



mkFSPTs :: S57FileRecord -> [FSPT]
mkFSPTs r 
  | ((structureFieldName . rootLabel $ r) /= "FSPT") = error $ "not an FSPT record: " ++ show r
  | otherwise = 
      let rv = structureMultiField . rootLabel $ r
          lookupFieldM k _r =
            maybe (error $ "mkFSPT: unable to lookup key " ++ T.unpack k)
            id $ Map.lookup k _r
          lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkFSPT _r = FSPT { _fsptName = lookupField "*NAME" _r
                           , _fsptOrientation = lookupField "ORNT" _r
                           , _fsptUsageIndicator = lookupField "USAG" _r
                           , _fsptMaskingIndicator = lookupField "MASK" _r
                           }
      in fmap mkFSPT rv


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
       , _fridFFPTs :: ! [FFPT]
       , _fridFSPC :: ! (Maybe FSPC)        
       , _fridFSPTs :: ! [FSPT]
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
                  , _fridFFPTs = maybe mempty mkFFPTs $ lookupChildFieldM "FRID" r "FFPT"
                  , _fridFSPC = fmap fromS57FileRecord $ lookupChildFieldM "FRID" r "FSPC"
                  , _fridFSPTs = maybe mempty mkFSPTs $ lookupChildFieldM "FRID" r "FSPT"
                  }

