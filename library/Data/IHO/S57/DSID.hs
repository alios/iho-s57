{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.DSID where

import Control.Lens
import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.IHO.S57.Types


data ExchangePurpose = New | Revision
                     deriving (Show, Eq, Data, Typeable)
                              
instance Enum ExchangePurpose where
  toEnum 1 = New
  toEnum 2 = Revision
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a ExchangePurpose"
  fromEnum New = 1
  fromEnum Revision = 2

data ProductSpecification = ENC | ODD
                     deriving (Show, Eq, Data, Typeable)
                              
instance Enum ProductSpecification where
  toEnum 1 = ENC
  toEnum 2 = ODD
  toEnum t = error $ "toEnum: " ++ show t ++ " is not a ProductSpecification"
  fromEnum ENC = 1
  fromEnum ODD = 2

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



data DSID =
  DSID { _dsidRecordName :: ! RecordName
       , _dsidExchangePurpose :: ! ExchangePurpose
       , _dsidIntendedUsage :: ! Int
       , _dsidDataSetName :: ! Text
       , _dsidEdition :: ! Text
       , _dsidUpdate :: ! Text
       , _dsidIssueDate :: ! Text
       , _dsidS57Edition :: ! Double
       , _dsidProductSpecification :: ! ProductSpecification
       , _dsidProductSpecificationDescrption :: ! Text
       , _dsidProductSpecificationEdition :: ! Text
       , _dsidApplicationProfile :: ! ApplicationProfile
       , _dsidProducingAgency :: ! Int
       } deriving (Show, Eq, Data, Typeable)
makeLenses ''DSID

instance HasRecordName DSID where
  recordName = dsidRecordName


instance FromS57FileRecord DSID where
    fromS57FileRecord r
      | ((structureFieldName . rootLabel $ r) /= "DSID") = error $ "not an DSID record: " ++ show r
      | otherwise =
        let rv = structureLinearField . rootLabel $ r
            lookupField k = fromS57Value $
                            maybe (error $ "DSID: unable to lookup key " ++ T.unpack k)
                            id $ Map.lookup k rv
            rn = RecordName { _rcnm = lookupField "RCNM", _rcid = lookupField "RCID" }




{-
  [Node{rootLabel = S57SingleValue "0001" (S57Int 1),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "DSID"
                    [("RCNM", S57Int 10), ("RCID", S57Int 1), ("EXPP", S57Int 2),
                     ("INTU", S57Int 5), ("DSNM", S57CharData "GB5X01SW.001"),
                     ("EDTN", S57CharData "1"), ("UPDN", S57CharData "1"),
                     ("UADT", S57CharData "        "), ("ISDT", S57CharData "20010509"),
                     ("STED", S57Real 3.1), ("PRSP", S57Int 1),
                     ("PSDN", S57CharData ""), ("PRED", S57CharData "2.0"),
                     ("PROF", S57Int 2), ("AGEN", S57Int 540),
                     ("COMT", S57CharData "")],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "DSSI"
                            [("DSTR", S57Int 2), ("AALL", S57Int 1), ("NALL", S57Int 1),
                             ("NOMR", S57Int 0), ("NOCR", S57Int 0), ("NOGR", S57Int 7),
                             ("NOLR", S57Int 0), ("NOIN", S57Int 3), ("NOCN", S57Int 0),
                             ("NOED", S57Int 0), ("NOFA", S57Int 0)],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 2),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "VRID"
                    [("RCNM", S57Int 110), ("RCID", S57Int 155), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57MultiValue "SG2D"
                            [[("*YCOO", S57Int (-813126481)), ("XCOO", S57Int 1523941508)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 3),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "VRID"
                    [("RCNM", S57Int 110), ("RCID", S57Int 156), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57MultiValue "SG2D"
                            [[("*YCOO", S57Int (-813126481)), ("XCOO", S57Int 1523817964)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 4),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "VRID"
                    [("RCNM", S57Int 110), ("RCID", S57Int 157), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57MultiValue "SG2D"
                            [[("*YCOO", S57Int (-813125749)), ("XCOO", S57Int 1523877000)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 5),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 908), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 75), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584953155),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 37), ("ATVL", S57CharData "")],
                             [("*ATTL", S57Int 75), ("ATVL", S57CharData "")],
                             [("*ATTL", S57Int 107), ("ATVL", S57CharData "4")],
                             [("*ATTL", S57Int 141), ("ATVL", S57CharData "(3)")],
                             [("*ATTL", S57Int 142), ("ATVL", S57CharData "10")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\155\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 6),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 909), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 75), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584960496),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 37), ("ATVL", S57CharData "")],
                             [("*ATTL", S57Int 75), ("ATVL", S57CharData "")],
                             [("*ATTL", S57Int 107), ("ATVL", S57CharData "4")],
                             [("*ATTL", S57Int 141), ("ATVL", S57CharData "(9)")],
                             [("*ATTL", S57Int 142), ("ATVL", S57CharData "15")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\156\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 7),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 910), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 144), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584953159),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 75), ("ATVL", S57CharData "2")],
                             [("*ATTL", S57Int 171), ("ATVL", S57CharData "11")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\155\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 8),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 911), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 144), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584960500),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 75), ("ATVL", S57CharData "2")],
                             [("*ATTL", S57Int 171), ("ATVL", S57CharData "10")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\156\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 9),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 914), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 159), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584917913),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 71), ("ATVL", S57CharData "2")],
                             [("*ATTL", S57Int 125), ("ATVL", S57CharData "2")],
                             [("*ATTL", S57Int 187), ("ATVL", S57CharData "3")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\157\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 10),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 912), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 14), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584953147),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 4), ("ATVL", S57CharData "4")],
                             [("*ATTL", S57Int 13), ("ATVL", S57CharData "2")],
                             [("*ATTL", S57Int 75), ("ATVL", S57CharData "2,6,2")],
                             [("*ATTL", S57Int 76), ("ATVL", S57CharData "1")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FFPT"
                            [[("*LNAM", S57Bits "\FS\STXC\173\221\"\US\ACK"),
                              ("RIND", S57Int 2), ("COMT", S57CharData "")],
                             [("*LNAM", S57Bits "\FS\STXG\173\221\"\US\ACK"),
                              ("RIND", S57Int 2), ("COMT", S57CharData "")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\155\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]},
   Node{rootLabel = S57SingleValue "0001" (S57Int 11),
        subForest =
          [Node{rootLabel =
                  S57LinearValue "FRID"
                    [("RCNM", S57Int 100), ("RCID", S57Int 913), ("PRIM", S57Int 1),
                     ("GRUP", S57Int 2), ("OBJL", S57Int 14), ("RVER", S57Int 1),
                     ("RUIN", S57Int 1)],
                subForest =
                  [Node{rootLabel =
                          S57LinearValue "FOID"
                            [("AGEN", S57Int 540), ("FIDN", S57Int 584960492),
                             ("FIDS", S57Int 1567)],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "ATTF"
                            [[("*ATTL", S57Int 4), ("ATVL", S57CharData "4")],
                             [("*ATTL", S57Int 13), ("ATVL", S57CharData "4")],
                             [("*ATTL", S57Int 75), ("ATVL", S57CharData "6,2,6")],
                             [("*ATTL", S57Int 76), ("ATVL", S57CharData "1")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FFPT"
                            [[("*LNAM", S57Bits "\FS\STX\240\201\221\"\US\ACK"),
                              ("RIND", S57Int 2), ("COMT", S57CharData "")],
                             [("*LNAM", S57Bits "\FS\STX\244\201\221\"\US\ACK"),
                              ("RIND", S57Int 2), ("COMT", S57CharData "")]],
                        subForest = []},
                   Node{rootLabel =
                          S57MultiValue "FSPT"
                            [[("*NAME", S57Bits "n\156\NUL\NUL\NUL"), ("ORNT", S57Int 255),
                              ("USAG", S57Int 255), ("MASK", S57Int 255)]],
                        subForest = []}]}]}]

-}
