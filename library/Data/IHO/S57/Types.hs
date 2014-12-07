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

data RecordName = RecordName {
  _rcnm :: Text,
  _rcid :: Int
  } deriving (Show, Eq, Data, Typeable)
makeClassy ''RecordName


type S57FileRecord = Tree S57Structure
type S57File = [S57FileRecord]

class (HasRecordName r) => FromS57FileRecord r where
  fromS57FileRecord :: S57FileRecord -> r
  


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
  S57LinearValue Text [(Text, S57Value)] |
  S57MultiValue  Text [[(Text, S57Value)]]
  deriving (Show, Eq, Data, Typeable)