{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.IHO.S57.VRID where


import           Control.Lens
import           Data.Data          (Data)
import           Data.IHO.S57.Types
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Tree
import           Data.Typeable      (Typeable)

data VRPC =
  VRPC { _vrpcUpdateInstruction  :: ! UpdateInstruction
       , _vrpcObjectPointerIndex :: ! Int
       , _vrpcObjectPointers     :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''VRPC

readVRPC :: Tree S57Structure -> VRPC
readVRPC r
    | (structureFieldName . rootLabel $ r) /= "VRPC" =
        error $ "not an VRPC record: " ++ show r
    | otherwise =
        VRPC { _vrpcUpdateInstruction = lookupField r "VPUI"
             , _vrpcObjectPointerIndex = lookupField r "VPIX"
             , _vrpcObjectPointers = lookupField r "NVPT"
             }


data TopologyIndicator
    = BeginningNode | EndNode | LeftFace | RightFace | ContainingFace | NullTopo
      deriving (Show, Eq, Data, Typeable)


data VRPT =
  VRPT { _vrptName              :: ! RecordName
       , _vrptOrientation       :: ! Orientation
       , _vrptUsageIndicator    :: ! UsageIndicator
       , _vrptTopologyIndicator :: ! TopologyIndicator
       , _vrptMaskingIndicator  :: ! MaskingIndicator
       } deriving (Show, Eq, Data, Typeable)

mkVRPTs :: S57FileRecord -> [VRPT]
mkVRPTs r
  | (structureFieldName . rootLabel $ r) /= "VRPT" = error $ "not an VRPT record: " ++ show r
  | otherwise =
      let rv = structureMultiField . rootLabel $ r
          lookupFieldM k _r =
            maybe (error $ "mkVRPT: unable to lookup key " ++ T.unpack k)
            id $ Map.lookup k _r
          _lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkVRPT _r = VRPT { _vrptName = _lookupField "*NAME" _r
                           , _vrptOrientation = _lookupField "ORNT" _r
                           , _vrptUsageIndicator = _lookupField "USAG" _r
                           , _vrptTopologyIndicator = _lookupField "TOPI" _r
                           , _vrptMaskingIndicator = _lookupField "MASK" _r
                           }
      in fmap mkVRPT rv

data SGCC =
  SGCC { _sgccUpdateInstruction :: ! UpdateInstruction
       , _sgccCoordinateIndex   :: ! Int
       , _sgccCoordinates       :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''SGCC

readSGCC :: Tree S57Structure -> SGCC
readSGCC r
    | ((structureFieldName . rootLabel $ r) /= "SGCC") =
        error $ "not an SGCC record: " ++ show r
    | otherwise =
        SGCC { _sgccUpdateInstruction = lookupField r "CCUI"
             , _sgccCoordinateIndex = lookupField r "CCIX"
             , _sgccCoordinates = lookupField r "CCNC"
             }

mkSG2Ds :: S57FileRecord -> [(Double, Double)]
mkSG2Ds =
  let mkSG2D lf _r = (lf "*YCOO" _r, lf "XCOO" _r)
  in mkTuples "mkSG2D" mkSG2D

mkSG3Ds :: S57FileRecord -> [(Double, Double, Double)]
mkSG3Ds =
  let mkSG3D lf _r = (lf "*YCOO" _r, lf "XCOO" _r, lf "VE3D" _r)
  in mkTuples "mkSG3D" mkSG3D



data VRID =
    VRID { _vridRecordName        :: ! RecordName
         , _vridVersion           :: ! Int
         , _vridUpdateInstruction :: UpdateInstruction
         , _vridATTFs             :: ! (Map Int Text)
         , _vridVRPC              :: ! (Maybe VRPC)
         , _vridVRPTs             :: ! [VRPT]
         , _vridSGCC              :: ! (Maybe SGCC)
         , _vridSG2Ds             :: ! [(Double, Double)]
         , _vridSG3Ds             :: ! [(Double, Double, Double)]
         }  deriving (Show, Eq, Data, Typeable)
makeLenses ''VRID

vridTableEmpty :: RecordTable VRID
vridTableEmpty = mempty

vridUpsert :: RecordTable VRID -> VRID -> RecordTable VRID
vridUpsert tbl v =
  let rv = v ^. vridVersion
      vui = v ^. vridUpdateInstruction
  in case vui of
   Insert ->
     if rv /= 1
     then error $ "vridUpsert: INSERT record with version != 1: " ++ show v
     else insertRecord v tbl
   Delete -> deleteRecord v tbl
   Modify ->
     let rn = v ^. recordName
         r = fromMaybe
             (error $ "vridUpdate: MODIFY for non existing record: " ++
              show rn) $ lookupRecord v tbl
         attfs = updateATTFs (r ^. vridATTFs) $ Map.toList $ v ^. vridATTFs
         vrpts = maybe (v ^. vridVRPTs) (updateVRPTs v r) $
                 pointerUpdateApplyable vridVRPC vridVRPTs r
         sg2ds = maybe (v ^. vridSG2Ds) (updateSG2Ds v r) $
                 pointerUpdateApplyable vridSGCC vridSG2Ds r
         sg3ds = maybe (v ^. vridSG3Ds) (updateSG3Ds v r) $
                 pointerUpdateApplyable vridSGCC vridSG2Ds r
         r' = r { _vridVersion = rv
                , _vridUpdateInstruction = vui
                , _vridATTFs = attfs
                , _vridVRPTs = vrpts
                , _vridSG2Ds = sg2ds
                , _vridSG3Ds = sg3ds
                }
     in if rv <= (r ^.vridVersion)
        then error $ "vridUpdate: MODIFY must have a version >= " ++ show rv
        else insertRecord r' tbl


updateVRPTs :: VRID -> VRID -> VRPC -> [VRPT]
updateVRPTs = updatePointerFields
              vrpcUpdateInstruction
              vrpcObjectPointerIndex
              vrpcObjectPointers
              vridVRPTs

updateBySGCC :: Getting [b] VRID [b] -> VRID -> VRID -> SGCC -> [b]
updateBySGCC = updatePointerFields
               sgccUpdateInstruction
               sgccCoordinateIndex
               sgccCoordinates

updateSG2Ds :: VRID -> VRID -> SGCC -> [(Double, Double)]
updateSG2Ds = updateBySGCC vridSG2Ds

updateSG3Ds :: VRID -> VRID -> SGCC -> [(Double, Double, Double)]
updateSG3Ds = updateBySGCC vridSG3Ds


instance FromS57FileRecord VRID where
  fromS57FileDataRecord r
    | ((structureFieldName . rootLabel $ r) /= "VRID") =
        error $ "not an VRID record: " ++ show r
    | otherwise =
        VRID { _vridRecordName = RecordName {
                  _rcnm = lookupField r "RCNM",
                  _rcid = lookupField r "RCID" }
             , _vridVersion = lookupField r "RVER"
             , _vridUpdateInstruction = lookupField r "RUIN"
             , _vridATTFs = maybe mempty mkAttrs $
                            lookupChildFieldM "VRID" r "ATTF"
             , _vridVRPC = fmap readVRPC $
                           lookupChildFieldM "VRID" r "FFPC"
             , _vridVRPTs = maybe mempty mkVRPTs $
                            lookupChildFieldM "VRID" r "VRPT"
             , _vridSGCC = fmap readSGCC $
                           lookupChildFieldM "VRID" r "SGCC"
             , _vridSG2Ds = maybe mempty mkSG2Ds $
                            lookupChildFieldM "VRID" r "SG2D"
             , _vridSG3Ds = maybe mempty mkSG3Ds $
                            lookupChildFieldM "VRID" r "SG3D"
             }

instance HasRecordName VRID where
  recordName = vridRecordName

instance Enum TopologyIndicator where
  toEnum 1 = BeginningNode
  toEnum 2 = EndNode
  toEnum 3 = LeftFace
  toEnum 4 = RightFace
  toEnum 5 = ContainingFace
  toEnum 255 = NullTopo
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a TopologyIndicator"
  fromEnum BeginningNode = 1
  fromEnum EndNode = 2
  fromEnum LeftFace = 3
  fromEnum RightFace = 4
  fromEnum ContainingFace = 5
  fromEnum NullTopo = 255

instance FromS57Value TopologyIndicator where
  fromS57Value (S57CharData "B") = BeginningNode
  fromS57Value (S57CharData "E") = EndNode
  fromS57Value (S57CharData "S") = LeftFace
  fromS57Value (S57CharData "D") = RightFace
  fromS57Value (S57CharData "F") = ContainingFace
  fromS57Value (S57CharData "N") = NullTopo
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value TopologyIndicator undefined for " ++ show v
