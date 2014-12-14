{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.DSID where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable, cast)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types




data DataStructure =
  CartographicSpaghetti |
  ChainNode |
  PlanarGraph |
  FullTopo |
  TopoIrrelevant
  deriving (Show, Eq, Data, Typeable)

instance Enum DataStructure where
  toEnum 1 = CartographicSpaghetti
  toEnum 2 = ChainNode
  toEnum 3 = PlanarGraph
  toEnum 4 = FullTopo
  toEnum 255 = TopoIrrelevant
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a DataStructure"
  fromEnum CartographicSpaghetti = 1
  fromEnum ChainNode = 2
  fromEnum PlanarGraph = 3
  fromEnum FullTopo = 4
  fromEnum TopoIrrelevant = 255


instance FromS57Value DataStructure where
  fromS57Value (S57CharData "CS") = CartographicSpaghetti
  fromS57Value (S57CharData "CN") = ChainNode
  fromS57Value (S57CharData "PG") = PlanarGraph
  fromS57Value (S57CharData "FT") = FullTopo
  fromS57Value (S57CharData "NO") = TopoIrrelevant
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value DataStructure undefined for " ++ show v


data DSSI = 
  DSSI { _dssiDataStructure :: ! DataStructure
       , _dssiATTFLexicalLevel :: ! Int
       , _dssiNATFLexicalLevel :: ! Int
       , _dssiMetaRecords :: ! Int
       , _dssiCartographicRecords :: ! Int
       , _dssiGeoRecords :: ! Int
       , _dssiCollectionRecords :: ! Int
       , _dssiIsolatedNodeRecords :: ! Int
       , _dssiConnectedNodeRecords :: ! Int
       , _dssiEdgeRecords :: ! Int
       , _dssiFaceRecords :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeClassy ''DSSI
         
readDSSI r
    | ((structureFieldName . rootLabel $ r) /= "DSSI") = error $ "not an DSSI record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "DSSI: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
        in   DSSI { _dssiDataStructure = lookupField "DSTR"
                  , _dssiATTFLexicalLevel = lookupField "AALL"
                  , _dssiNATFLexicalLevel = lookupField "NALL"
                  , _dssiMetaRecords = lookupField "NOMR"
                  , _dssiCartographicRecords = lookupField "NOCR"
                  , _dssiGeoRecords = lookupField "NOGR"
                  , _dssiCollectionRecords = lookupField "NOLR"
                  , _dssiIsolatedNodeRecords = lookupField "NOIN"
                  , _dssiConnectedNodeRecords = lookupField "NOCN"
                  , _dssiEdgeRecords = lookupField "NOED"
                  , _dssiFaceRecords = lookupField "NOFA"
                  }



data ExchangePurpose = New | Revision
                     deriving (Show, Eq, Data, Typeable)
                              
instance Enum ExchangePurpose where
  toEnum 1 = New
  toEnum 2 = Revision
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a ExchangePurpose"
  fromEnum New = 1
  fromEnum Revision = 2


instance FromS57Value ExchangePurpose where
  fromS57Value (S57CharData "N") = New
  fromS57Value (S57CharData "R") = Revision
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value ExchangePurpose undefined for " ++ show v
  
data ProductSpecification = ENC | ODD
                     deriving (Show, Eq, Data, Typeable)
                              
instance Enum ProductSpecification where
  toEnum 1 = ENC
  toEnum 2 = ODD
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a ProductSpecification"
  fromEnum ENC = 1
  fromEnum ODD = 2

instance FromS57Value ProductSpecification where
  fromS57Value (S57CharData "ENC") = ENC
  fromS57Value (S57CharData "ODD") = ODD
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value ProducatSpecification undefined for " ++ show v



data ApplicationProfile = ENCNew | ENCRevision | IHODataDictionary
                     deriving (Show, Eq, Data, Typeable)
                              
instance Enum ApplicationProfile where
  toEnum 1 = ENCNew
  toEnum 2 = ENCRevision
  toEnum 3 = IHODataDictionary
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a ApplicationProfile"
  fromEnum ENCNew = 1
  fromEnum ENCRevision = 2
  fromEnum IHODataDictionary = 3

instance FromS57Value ApplicationProfile where
  fromS57Value (S57CharData "EN") = ENCNew
  fromS57Value (S57CharData "ER") = ENCRevision
  fromS57Value (S57CharData "DD") = IHODataDictionary
  fromS57Value (S57Int i) = toEnum i
  fromS57Value v = error $ "fromS57Value ProducatSpecification undefined for " ++ show v


data DSID =
  DSID { _dsidRecordName :: ! RecordName
       , _dsidExchangePurpose :: ! ExchangePurpose
       , _dsidIntendedUsage :: ! Int
       , _dsidDataSetName :: ! Text
       , _dsidEdition :: ! Text
       , _dsidUpdate :: ! Text
       , _dsidUpdateApplicationDate :: ! Text
       , _dsidIssueDate :: ! Text
       , _dsidS57Edition :: ! Double
       , _dsidProductSpecification :: ! ProductSpecification
       , _dsidProductSpecificationDescrption :: ! Text
       , _dsidProductSpecificationEdition :: ! Text
       , _dsidApplicationProfile :: ! ApplicationProfile
       , _dsidProducingAgency :: ! Int
       , _dsidDSSI :: ! DSSI
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''DSID

instance HasRecordName DSID where
  recordName = dsidRecordName

instance HasDSSI DSID where
  dSSI = dsidDSSI

instance FromS57FileRecord DSID where
  fromS57FileRecord r
    | ((structureFieldName . rootLabel $ r) /= "DSID") = error $ "not an DSID record: " ++ show r
    | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupFieldM k =
              maybe (error $ "DSID: unable to lookup key " ++ T.unpack k)
              id $ Map.lookup k rv
            lookupField k = fromS57Value $ lookupFieldM k
            rn = RecordName { _rcnm = lookupField "RCNM", _rcid = lookupField "RCID" }
        in   DSID { _dsidRecordName = rn
                  , _dsidExchangePurpose = lookupField "EXPP"
                  , _dsidIntendedUsage = lookupField "INTU"
                  , _dsidDataSetName = lookupField "DSNM"
                  , _dsidEdition = lookupField "EDTN"
                  , _dsidUpdate = lookupField "UPDN"
                  , _dsidUpdateApplicationDate = lookupField "UADT"
                  , _dsidIssueDate = lookupField "ISDT"
                  , _dsidS57Edition = lookupField "STED"
                  , _dsidProductSpecification = lookupField "PRSP"
                  , _dsidProductSpecificationDescrption = lookupField "PSDN"
                  , _dsidProductSpecificationEdition = lookupField "PRED"
                  , _dsidApplicationProfile = lookupField "PROF"
                  , _dsidProducingAgency = lookupField "AGEN"
                  , _dsidDSSI = readDSSI $ lookupChildField "DSID" r "DSSI"
                  }



toDSID :: S57FileRecord -> Maybe DSID
toDSID r = cast $ (fromS57FileRecord r :: DSID)

isDSID :: S57FileRecord -> Bool
isDSID = maybe False (\_ -> True) . toDSID
