{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.Types where

import Control.Lens
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text)
import Data.Tree
import Data.ByteString (ByteString)
import Data.Map (Map)


data RecordNameT =
  CD | DP
  deriving (Show, Eq, Data, Typeable)

instance Enum RecordNameT where
  fromEnum CD = error "CD not defined for binary use"
  fromEnum DP = 20
  toEnum 20 = DP
  toEnum n = error $ "toEnum: undefined RecordNameT: " ++ show n

data RecordName = RecordName {
  _rcnm :: RecordNameT,
  _rcid :: Int
  } deriving (Show, Eq, Data, Typeable)
makeClassy ''RecordName


type S57FileRecord = Tree S57Structure
type S57File = [S57FileRecord]

class FromS57FileRecord r where
  fromS57FileRecord :: S57FileRecord -> r
  
instance FromS57Value RecordNameT where
  fromS57Value (S57CharData "DP") = DP
  fromS57Value (S57CharData "CD") = CD
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value RecordNameT undefined for " ++ show v



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
  fromS57Value t = error $ "unable to convert value to Double: " ++ show t

instance FromS57Value ByteString where
  fromS57Value (S57Bits t) = t
  fromS57Value t = error $ "unable to convert value to Double: " ++ show t



data S57Structure =
  S57SingleValue Text S57Value |
  S57LinearValue Text (Map Text S57Value)|
  S57MultiValue  Text [Map Text S57Value]
  deriving (Show, Eq, Data, Typeable)


structureFieldName :: S57Structure -> Text
structureFieldName (S57SingleValue fn _) = fn
structureFieldName (S57LinearValue fn _) = fn
structureFieldName (S57MultiValue  fn _) = fn

structureSingleField (S57SingleValue _ v) = v
structureLinearField (S57LinearValue _ v) = v
structureMultiField  (S57MultiValue  _ v) = v


dropISO :: S57FileRecord -> S57FileRecord
dropISO = head . dropParent "0001"


dropParent :: Text -> S57FileRecord -> [S57FileRecord]
dropParent p n
  | ((structureFieldName . rootLabel $ n) /= p) = error $ "dropParent wrong name: " ++ show n
  | otherwise = subForest n


lookupChildFields :: Text -> S57FileRecord -> Text -> [Tree S57Structure]
lookupChildFields p n fn =
  let cf c = fn == (structureFieldName . rootLabel $ c)
  in filter cf $ dropParent p n

lookupChildField :: Text -> S57FileRecord -> Text -> Tree S57Structure
lookupChildField p n fn = head $ lookupChildFields p n fn
