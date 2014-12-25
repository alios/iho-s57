{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}


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
import Data.Monoid
import Data.Table
import Control.Applicative

data FOID =
  FOID { _foidProducingAgency :: ! Int
       , _foidIdentificationNumber :: ! Int
       , _foidFeatureIdentificationSub :: ! Int
       } deriving (Show, Eq, Data, Typeable)

makeClassy ''FOID


readFOID :: Tree S57Structure -> FOID
readFOID r
    | ((structureFieldName . rootLabel $ r) /= "FOID") =
        error $ "not an FOID record: " ++ show r
    | otherwise =
        FOID { _foidProducingAgency = lookupField r "AGEN"
             , _foidIdentificationNumber = lookupField r "FIDN"
             , _foidFeatureIdentificationSub = lookupField r "FIDS"
             }

data FFPC =
  FFPC { _ffpcUpdateInstruction :: ! UpdateInstruction
       , _ffpcObjectPointerIndex :: ! Int
       , _ffpcObjectPointers :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''FFPC

readFFPC :: Tree S57Structure -> FFPC
readFFPC r
    | ((structureFieldName . rootLabel $ r) /= "FFPC") =
        error $ "not an FFPC record: " ++ show r
    | otherwise =
        FFPC { _ffpcUpdateInstruction = lookupField r "FFUI"
             , _ffpcObjectPointerIndex = lookupField r "FFIX"
             , _ffpcObjectPointers = lookupField r "NFPT"
             }

data RelationShipIndicator =
  Master | Slave | Peer
  deriving (Show, Eq, Data, Typeable)


data FFPT =
  FFPT { _ffptLongName :: ! RecordName
       , _ffptRelationShipIndicator :: ! RelationShipIndicator
       , _ffptComment :: ! Text
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''FFPT

mkFFPTs :: S57FileRecord -> [FFPT]
mkFFPTs r 
  | ((structureFieldName . rootLabel $ r) /= "FFPT") =
      error $ "not an FFPT record: " ++ show r
  | otherwise = 
      let rv = structureMultiField . rootLabel $ r
          lookupFieldM k _r =
            maybe (error $ "mkFFPTs: unable to lookup key " ++ T.unpack k)
            id $ Map.lookup k _r
          _lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkFFPT _r = FFPT { _ffptLongName = _lookupField "*LNAM" _r
                           , _ffptRelationShipIndicator = _lookupField "RIND" _r
                           , _ffptComment = _lookupField "COMT" _r
                           }
      in fmap mkFFPT rv

data FSPC =
  FSPC { _fspcUpdateInstruction :: ! UpdateInstruction
       , _fspcSpatialPointerIndex :: ! Int
       , _fspcSpatialPointers :: ! Int
       } deriving (Show, Eq, Data, Typeable)

makeClassy ''FSPC

readFSPC :: Tree S57Structure -> FSPC
readFSPC r
    | ((structureFieldName . rootLabel $ r) /= "FSPC") =
        error $ "not an FSPC record: " ++ show r
    | otherwise =
        FSPC { _fspcUpdateInstruction = lookupField r "FSUI"
             , _fspcSpatialPointerIndex = lookupField r "FSIX"
             , _fspcSpatialPointers = lookupField r "NSPT"
             }

data FSPT =
  FSPT { _fsptName :: ! RecordName
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
          _lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkFSPT _r = FSPT { _fsptName = _lookupField "*NAME" _r
                           , _fsptOrientation = _lookupField "ORNT" _r
                           , _fsptUsageIndicator = _lookupField "USAG" _r
                           , _fsptMaskingIndicator = _lookupField "MASK" _r
                           }
      in fmap mkFSPT rv


data GeometricPrimitive = Point | Line | Area | NoRef
  deriving (Show, Eq, Data, Typeable)

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


instance Tabular FRID where
  type PKT FRID = RecordName 
  data Key k FRID b where
    FRIDRecordName :: Key Primary FRID RecordName
  data Tab FRID i = FRIDTab (i Primary RecordName)

  fetch FRIDRecordName = view recordName

  primary = FRIDRecordName
  primarily FRIDRecordName rn = rn

  mkTab f
    = FRIDTab <$> f FRIDRecordName
  forTab (FRIDTab rn) f
    = FRIDTab <$> f FRIDRecordName rn
  ixTab  (FRIDTab rn) FRIDRecordName = rn


fridUpsert :: Table FRID -> FRID -> Table FRID
fridUpsert tbl v =
  let rv = v ^. fridVersion
      vui = v ^. fridUpdateInstruction
  in case (vui) of
   Insert ->
     if (rv /= 1)
     then error $ "fridUpsert: INSERT record with version != 1: " ++ show v
     else insert v tbl
   Delete -> delete v tbl
   Modify ->
     let rn = v ^. recordName            
         r = maybe
             (error $ "fridUpdate: MODIFY for non existing record: " ++
              show rn) id $ lookupFrid tbl rn
         attfs = updateATTFs (r ^. fridATTFs) $ Map.toList $ v ^. fridATTFs
         natfs = updateATTFs (r ^. fridNATFs) $ Map.toList $ v ^. fridNATFs
         ffpts = maybe (v ^. fridFFPTs) (updateFFPTs v r) $
                 pointerUpdateApplyable fridFFPC fridFFPTs r     
         fspts = maybe (v ^. fridFSPTs) (updateFSPTs v r) $
                 pointerUpdateApplyable fridFSPC fridFSPTs r     

         r' = r { _fridVersion = rv
                , _fridUpdateInstruction = vui
                , _fridATTFs = attfs
                , _fridNATFs = natfs
                , _fridFFPTs = ffpts
                , _fridFSPTs = fspts
                }
     in if (rv <= (r ^.fridVersion))          
        then error $ "fridUpdate: MODIFY must have a version >= " ++ show rv 
        else insert r' tbl



updateFFPTs :: FRID -> FRID -> FFPC -> [FFPT]
updateFFPTs = updatePointerFields 
              ffpcUpdateInstruction
              ffpcObjectPointerIndex
              ffpcObjectPointers
              fridFFPTs

updateFSPTs :: FRID -> FRID -> FSPC -> [FSPT]
updateFSPTs = updatePointerFields 
              fspcUpdateInstruction
              fspcSpatialPointerIndex
              fspcSpatialPointers
              fridFSPTs


lookupFrid :: Table FRID -> RecordName -> Maybe FRID
lookupFrid tbl rn =
  case ((tbl ^. with FRIDRecordName (==) rn) ^. from table) of
   [] -> Nothing
   (x:_) -> Just x




instance HasFOID FRID where
  fOID = fridFOID

instance HasRecordName FRID where
  recordName = fridRecordName

instance FromS57FileRecord FRID where
  fromS57FileDataRecord r
    | ((structureFieldName . rootLabel $ r) /= "FRID") =
        error $ "not an FRID record: " ++ show r
    | otherwise =
        let groupV = lookupField r "GRUP"
        in FRID { _fridRecordName = RecordName {
                     _rcnm = lookupField r "RCNM",
                     _rcid = lookupField r "RCID" }
                , _fridGeometricPrimtive = lookupField r "PRIM"
                , _fridGroup = if ((groupV >= 1) && (groupV <=254))
                               then Just groupV else Nothing
                , _fridObjectLabel = lookupField r "OBJL"
                , _fridVersion = lookupField r "RVER"
                , _fridUpdateInstruction = lookupField r "RUIN"
                , _fridFOID = readFOID $ lookupChildField "FRID" r "FOID"
                , _fridATTFs = maybe mempty mkAttrs $
                               lookupChildFieldM "FRID" r "ATTF"
                , _fridNATFs = maybe mempty mkAttrs $
                               lookupChildFieldM "FRID" r "NATF"
                , _fridFFPC = fmap readFFPC $
                              lookupChildFieldM "FRID" r "FFPC"
                , _fridFFPTs = maybe mempty mkFFPTs $
                               lookupChildFieldM "FRID" r "FFPT"
                , _fridFSPC = fmap readFSPC $
                              lookupChildFieldM "FRID" r "FSPC"
                , _fridFSPTs = maybe mempty mkFSPTs $
                               lookupChildFieldM "FRID" r "FSPT"
                }

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

