{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.IHO.S57.Types where

import           Control.Lens
import           Data.Binary.Get
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (chr)
import           Data.Data            (Data)
import           Data.Hashable
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HashMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Tree
import           Data.Typeable        (Typeable)
import           GHC.Generics


data RecordNameT =
  CD | DS | DP | FE | IsolatedNode | ConnectedNode | Edge | Face
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Hashable RecordNameT

data RecordName = RecordName {
  _rcnm :: RecordNameT,
  _rcid :: Int
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)
makeClassy ''RecordName

instance Hashable RecordName


data Record r = Record {
  _recName :: RecordName,
  _recData :: r
  }
makeLenses ''Record

type RecordTable a = HashMap RecordName a

insertRecord, deleteRecord :: HasRecordName v => v -> RecordTable v -> RecordTable v
insertRecord v = HashMap.insert (v ^. recordName) v
deleteRecord v = HashMap.delete (v ^. recordName)
lookupRecord :: HasRecordName v => v -> RecordTable v -> Maybe v
lookupRecord v = HashMap.lookup (v ^. recordName)


instance HasRecordName (Record r) where
  recordName = recName

type S57FileRecord = Tree S57Structure
type S57File = [S57FileRecord]

class (Typeable r) => FromS57FileRecord r where
  fromS57FileDataRecord :: S57FileRecord -> r


fromS57FileRecord :: (FromS57FileRecord r) => S57FileRecord -> Record r
fromS57FileRecord r =
  let rn = readRecordName r
      d = fromS57FileDataRecord r
  in Record rn d

instance FromS57Value RecordNameT where
  fromS57Value (S57CharData "DS") = DS
  fromS57Value (S57CharData "DP") = DP
  fromS57Value (S57CharData "CD") = CD
  fromS57Value (S57CharData "FE") = FE
  fromS57Value (S57CharData "VI") = IsolatedNode
  fromS57Value (S57CharData "VC") = ConnectedNode
  fromS57Value (S57CharData "VE") = Edge
  fromS57Value (S57CharData "VF") = Face
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value RecordNameT undefined for " ++ show v


instance Enum RecordNameT where
  fromEnum CD = error "CD not defined for binary use"
  fromEnum DS = 10
  fromEnum DP = 20
  fromEnum FE = 100
  fromEnum IsolatedNode = 110
  fromEnum ConnectedNode = 120
  fromEnum Edge = 130
  fromEnum Face = 140
  toEnum 10 = DP
  toEnum 20 = DS
  toEnum 100 = FE
  toEnum 110 = IsolatedNode
  toEnum 120 = ConnectedNode
  toEnum 130 = Edge
  toEnum 140 = Face
  toEnum n = error $ "toEnum: undefined RecordNameT: " ++ show n


data S57Value =
  S57CharData Text |
  S57Int Int |
  S57Real Double |
  S57Bits ByteString
  deriving (Eq, Show, Data, Typeable)

class FromS57Value t where
  fromS57Value :: S57Value -> t

instance FromS57Value Text where
  fromS57Value (S57CharData t) = t
  fromS57Value t = error $ "unable to convert value to Text: " ++ show t

instance FromS57Value Int where
  fromS57Value (S57Int t) = t
  fromS57Value t = error $ "unable to convert value to Int: " ++ show t

instance FromS57Value Double where
  fromS57Value (S57Real t) = t
  fromS57Value (S57Int t) = fromIntegral t
  fromS57Value t = error $ "unable to convert value to Double: " ++ show t

instance FromS57Value ByteString where
  fromS57Value (S57Bits t) = t
  fromS57Value t = error $ "unable to convert value to ByteString: " ++ show t



data S57Structure =
  S57SingleValue Text S57Value |
  S57LinearValue Text (Map Text S57Value)|
  S57MultiValue  Text [Map Text S57Value]
  deriving (Show, Eq, Data, Typeable)


structureFieldName :: S57Structure -> Text
structureFieldName (S57SingleValue fn _) = fn
structureFieldName (S57LinearValue fn _) = fn
structureFieldName (S57MultiValue  fn _) = fn

structureSingleField :: S57Structure -> S57Value
structureSingleField (S57SingleValue _ v) = v
structureSingleField f = error $ "structureSingleField: unexpected " ++ show f

structureLinearField :: S57Structure -> Map Text S57Value
structureLinearField (S57LinearValue _ v) = v
structureLinearField f = error $ "structureLinearField: unexpected " ++ show f

structureMultiField :: S57Structure -> [Map Text S57Value]
structureMultiField  (S57MultiValue  _ v) = v
structureMultiField f = error $ "structureMultiField: unexpected " ++ show f



data UpdateInstruction = Insert | Delete | Modify
  deriving (Show, Eq, Data, Typeable)

instance FromS57Value UpdateInstruction where
  fromS57Value (S57CharData "I") = Insert
  fromS57Value (S57CharData "D") = Delete
  fromS57Value (S57CharData "M") = Modify
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value UpdateInstruction undefined for " ++ show v

data Orientation
  = Forward | Reverse | NullOrientation
  deriving (Show, Eq, Data, Typeable)

data UsageIndicator
  = Exterior | Interior | TruncatedExterior | NullUsage
  deriving (Show, Eq, Data, Typeable)

data MaskingIndicator
  = Mask | Show | NullMask
  deriving (Show, Eq, Data, Typeable)



mkAttrs :: S57FileRecord -> Map Int Text
mkAttrs r =
  let rv = structureMultiField . rootLabel $ r
      lookupFieldM k _r =
        maybe (error $ "mkAttrs: unable to lookup key " ++ T.unpack k)
        id $ Map.lookup k _r
      _lookupField k _r = fromS57Value $ lookupFieldM k _r
      mkATTF _r = (_lookupField "*ATTL" _r, _lookupField "ATVL" _r)
  in Map.fromList $ fmap mkATTF rv


mkTuples ::
  FromS57Value t =>
  String
  -> ((Text -> Map Text S57Value -> t) -> Map Text S57Value -> b)
  -> Tree S57Structure
  -> [b]
mkTuples txt mkTuple r =
  let rv = structureMultiField . rootLabel $ r
      lookupFieldM k _r =
        maybe (error $ txt ++ ": unable to lookup key " ++ T.unpack k)
        id $ Map.lookup k _r
      _lookupField k _r = fromS57Value $ lookupFieldM k _r
  in fmap (mkTuple _lookupField) rv


dropISO :: S57FileRecord -> S57FileRecord
dropISO = head . dropParent "0001"


dropParent :: Text -> S57FileRecord -> [S57FileRecord]
dropParent p n
  | ((structureFieldName . rootLabel $ n) /= p) = error $ "dropParent wrong name: " ++ show n
  | otherwise = subForest n


lookupRecords :: Text -> [S57FileRecord] -> ([S57FileRecord],[S57FileRecord])
lookupRecords rn rs =
  let fin = filter (\r ->(structureFieldName . rootLabel $ r) == rn) rs
      fout = filter (\r ->(structureFieldName . rootLabel $ r) /= rn) rs
  in (fin,fout)


lookupChildFields :: Text -> S57FileRecord -> Text -> [Tree S57Structure]
lookupChildFields p n fn =
  let cf c = fn == (structureFieldName . rootLabel $ c)
  in filter cf $ dropParent p n

lookupChildField :: Text -> S57FileRecord -> Text -> Tree S57Structure
lookupChildField p n fn = maybe (error $ "lookupChildField: unable to find: " ++ show fn) id $
                          lookupChildFieldM p n fn

lookupChildFieldM :: Text -> S57FileRecord -> Text -> Maybe (Tree S57Structure)
lookupChildFieldM p n fn =
  case (lookupChildFields p n fn) of
   [] -> Nothing
   (x:_) -> Just x

lookupField :: FromS57Value t => Tree S57Structure -> Text -> t
lookupField r =
  let rv = structureLinearField . rootLabel $ r
      lookupFieldM k =
        maybe (error $ "lookupField: unable to lookup key " ++ T.unpack k)
        id $ Map.lookup k rv
  in fromS57Value . lookupFieldM

readRecordName :: Tree S57Structure -> RecordName
readRecordName r =
  RecordName { _rcnm = lookupField r "RCNM"
             , _rcid = lookupField r "RCID" }

updateATTFs :: Map Int Text -> [(Int, Text)] -> Map Int Text
updateATTFs = foldl updateATTFs'

updateATTFs' :: Map Int Text -> (Int, Text) -> Map Int Text
updateATTFs' m (i, t) =
  if (t == s57deleteChar) then Map.delete i m else Map.insert i t m

s57deleteChar :: Text
s57deleteChar =  T.singleton $ chr 0x7f

updatePointerFields ::
  Show a =>
  Getting UpdateInstruction s UpdateInstruction
  -> Getting Int s Int
  -> Getting Int s Int
  -> Getting [b] a [b]
  -> a
  -> a
  -> s
  -> [b]
updatePointerFields pui pidx pn g v r vrpc =
  let upP = vrpc ^. pidx
      upN = vrpc ^. pn
      rpcUI = vrpc ^. pui
      rpts' = v ^. g
      rptsN =
        let rs = r ^. g
        in if ((rpcUI /= Delete) && (length rs /= upN))
           then error $ mconcat ["updatePointerFields: ",show rpcUI
                                ," but wrong number of subfields: "
                                , show r
                                ]
           else rs
      in case (rpcUI) of
        Insert -> mconcat [take upP rpts', rptsN, drop upP rpts']
        Delete -> mconcat [take upP rpts', drop (upP + upN ) rpts']
        Modify -> mconcat [take upP rpts'
                          ,rptsN
                          ,drop (upP + upN ) rpts']


pointerUpdateApplyable :: Getting (Maybe a) s (Maybe a)
                       -> Getting [t] s [t]
                       -> s -> Maybe a
pointerUpdateApplyable updateInst updateVal r =
  let updateable =
        case (r ^. updateInst, r ^. updateVal) of
         (Nothing, _) -> Nothing
         (Just _, []) -> Nothing
         (Just ccj, _) -> Just ccj
  in updateable



instance FromS57Value RecordName where
  fromS57Value (S57Bits bs) =
    let getRNFs = runGet $ do
          a <- getWord8
          b <- getWord32le
          return (toEnum $ fromIntegral a, fromIntegral b)
        (rn, rid) = getRNFs $ BL.fromStrict bs
    in RecordName { _rcnm = rn, _rcid = rid }
  fromS57Value v = error $ "fromS57Value UsageIndicator undefined for " ++ show v



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


instance Enum UpdateInstruction where
  toEnum 1 = Insert
  toEnum 2 = Delete
  toEnum 3 = Modify
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a UpdateInstruction"
  fromEnum Insert = 1
  fromEnum Delete = 2
  fromEnum Modify = 3


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
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value Orientation undefined for " ++ show v
