{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.VRID where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable, cast)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types
import Data.Map (Map)
import Data.Monoid
import Data.ByteString (ByteString)


data VRPC =
  VRPC { _vrpcUpdateInstruction :: ! UpdateInstruction
       , _vrpcObjectPointerIndex :: ! Int
       , _vrpcFeatureObjectPointers :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''VRPC


readVRPC r
    | ((structureFieldName . rootLabel $ r) /= "VRPC") = error $ "not an VRPC record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "VRPC: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
        in VRPC { _vrpcUpdateInstruction = lookupField "VPUI"
                , _vrpcObjectPointerIndex = lookupField "VPIX"
                , _vrpcFeatureObjectPointers = lookupField "NVPT"
                }



data TopologyIndicator
    = BeginningNode | EndNode | LeftFace | RightFace | ContainingFace | NullTopo
      deriving (Show, Eq, Data, Typeable)

                      
      
data VRPT =
  VRPT { _vrptName :: ! ByteString
       , _vrptOrientation :: ! Orientation
       , _vrptUsageIndicator :: ! UsageIndicator
       , _vrptTopologyIndicator :: ! TopologyIndicator
       , _vrptMaskingIndicator :: ! MaskingIndicator
       } deriving (Show, Eq, Data, Typeable)

mkVRPTs :: S57FileRecord -> [VRPT]
mkVRPTs r 
  | ((structureFieldName . rootLabel $ r) /= "VRPT") = error $ "not an VRPT record: " ++ show r
  | otherwise = 
      let rv = structureMultiField . rootLabel $ r
          lookupFieldM k _r =
            maybe (error $ "mkVRPT: unable to lookup key " ++ T.unpack k)
            id $ Map.lookup k _r
          lookupField k _r = fromS57Value $ lookupFieldM k _r
          mkVRPT _r = VRPT { _vrptName = lookupField "*NAME" _r
                           , _vrptOrientation = lookupField "ORNT" _r
                           , _vrptUsageIndicator = lookupField "USAG" _r
                           , _vrptTopologyIndicator = lookupField "TOPI" _r
                           , _vrptMaskingIndicator = lookupField "MASK" _r
                           }
      in fmap mkVRPT rv
 
data VRID =
    VRID { _vridRecordName :: ! RecordName
         , _vridVersion :: ! Int
         , _vridUpdateInstruction :: UpdateInstruction
         , _vridATTFs :: ! (Map Int Text)
         , _vridVRPC :: ! (Maybe VRPC)
         , _vridVRPTs :: ! [VRPT]                       
         }  deriving (Show, Eq, Data, Typeable)
makeLenses ''VRID

    
instance HasRecordName VRID where
    recordName = vridRecordName

instance FromS57FileRecord VRID where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "FRID") = error $ "not an FRID record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "FRID: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
            rn = RecordName { _rcnm = lookupField "RCNM", _rcid = lookupField "RCID" }
        in VRID { _vridRecordName = rn
                , _vridVersion = lookupField "RVER"
                , _vridUpdateInstruction = lookupField "RUIN"
                , _vridATTFs = maybe mempty mkAttrs $ lookupChildFieldM "VRID" r "ATTF"
                , _vridVRPC = fmap readVRPC $ lookupChildFieldM "VRID" r "FFPC"
                , _vridVRPTs = maybe mempty mkVRPTs $ lookupChildFieldM "VRID" r "VRPT"                             
                }

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
 

toVRPC :: S57FileRecord -> Maybe VRPC
toVRPC r = cast $ (readVRPC r :: VRPC)

isVRPC :: S57FileRecord -> Bool
isVRPC = maybe False (\_ -> True) . toVRPC
