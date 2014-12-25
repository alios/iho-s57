{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.IHO.S57.Reader
       ( S57Catalog (..), s57readCatalog
       , S57DataSet (..), dataSetVRIDTable, dataSetFRIDTable
       , S57Record (..)      
       , s57Conduit
       , s57FileSource
       , s57readDataSet) where
import Control.Lens
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Control.Monad.RWS
import Data.Conduit
import Data.Conduit.Lift
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Binary as CB
import Control.Monad.Trans.Resource
import Data.Table
import Data.Typeable (Typeable)

import Data.IHO.S57.Types
import Data.IHO.S57.Parser
import Data.IHO.S57.CATD
import Data.IHO.S57.DSID
import Data.IHO.S57.DSPM
import Data.IHO.S57.FRID
import Data.IHO.S57.VRID    

data S57Record =
  RecordCATD CATD |
  RecordDSID DSID |
  RecordDSPM DSPM |
  RecordVRID VRID |
  RecordFRID FRID 
  deriving (Show, Eq)

data ReaderState =
  ReaderState { _ddr :: Maybe DDR
              , _lexConfig :: LexLevelConfig
              } 
makeLenses ''ReaderState


ddrSink :: (MonadThrow m) => Consumer ByteString m DDR                       
ddrSink = sinkParser parseDDR

drSink :: (MonadThrow m) =>
           DDR -> LexLevelConfig -> Consumer ByteString m (Maybe [S57Structure])
drSink _ddr ll = sinkParser $ parseDR _ddr ll

data S57DataSet =
  S57DataSet { _dataSetFRIDTable :: Table FRID
             , _dataSetVRIDTable :: Table VRID
             } deriving (Show, Eq, Typeable)
makeLenses ''S57DataSet

data S57Catalog =
  S57Catalog [CATD] deriving (Show, Eq, Typeable)



s57readCatalog :: (Monad m) => Consumer S57Record m S57Catalog
s57readCatalog = s57readCatalog' $ S57Catalog mempty

s57readCatalog' :: (Monad m) => S57Catalog -> Consumer S57Record m S57Catalog
s57readCatalog' cat@(S57Catalog catds) = do
  v <- await
  case v of
   Nothing -> return cat
   Just (RecordCATD r) -> s57readCatalog' $ S57Catalog (r:catds)
   Just r -> fail $ "s57readCatalog: unexpected record: " ++ show r

s57readDataSet :: (Monad m) => Consumer S57Record m S57DataSet
s57readDataSet = readDataSet' $
              S57DataSet { _dataSetFRIDTable = EmptyTable
                         , _dataSetVRIDTable = EmptyTable
                         }

readDataSet' :: (Monad m) => S57DataSet -> Consumer S57Record m S57DataSet
readDataSet' ds = do
  v <- await
  let ds' = case v of
        Nothing -> Nothing
        Just (RecordVRID r) -> Just
          ds { _dataSetVRIDTable = vridUpsert (ds ^. dataSetVRIDTable) r }
        Just (RecordFRID r) -> Just
          ds { _dataSetFRIDTable = fridUpsert (ds ^. dataSetFRIDTable) r }
        _ -> Just ds   
  case ds' of
    Nothing -> return ds
    Just ds_ -> readDataSet' ds_
     
s57ConduitS :: (MonadState ReaderState m, MonadThrow m) =>
               Conduit ByteString m S57Record
s57ConduitS = do
  ll <- fmap _lexConfig get
  ddrM <- fmap (view ddr) get
  ddrF <- case (ddrM) of
              Nothing -> do
                ddr' <- ddrSink
                modify (\st -> st { _ddr = Just ddr' })
                return ddr'
              Just ddr' -> return ddr'
  drM <- drSink ddrF ll
  case drM of
   Nothing -> return ()
   Just dr' ->
     let dr = dropISO . readDRs ddrF $  dr'
     in do r <- handleRecord dr
           yield r
           s57ConduitS



s57Conduit :: (MonadThrow m) => Conduit ByteString m S57Record
s57Conduit =
  evalStateLC (ReaderState Nothing defaultLexLevelConfig) $ s57ConduitS

handleRecord :: (MonadState ReaderState m) => S57FileRecord -> m S57Record
handleRecord dr =
  let rn = readRecordName dr
      readVRID st =
        let ll = st ^. lexConfig
            vrid = fixVRID ll . fromS57FileDataRecord $ dr
        in RecordVRID vrid
      fixVRID ll v =
        let cf = fromIntegral $ lexLevelCoordinateMulFactor ll
            sf = fromIntegral $ lexLevelSoundingMulFactor ll
            sg2ds = fmap (\(x,y) -> (x / cf, y / cf)) (v ^. vridSG2Ds)
            sg3ds = fmap (\(x,y,z) -> (x / cf, y / cf, z / sf)) (v ^. vridSG3Ds)
        in v { _vridSG2Ds = sg2ds, _vridSG3Ds = sg3ds }
  in case (rn ^. rcnm) of
      CD -> return . RecordCATD . fromS57FileDataRecord $ dr
      DP -> do
        st <- get
        let dsid' = (fromS57FileDataRecord $ dr) :: DSID
            dssi' = dsid' ^. dsidDSSI
            lexConfig' = (st ^. lexConfig){ lexLevelATTF =
                                               dssi' ^. dssiATTFLexicalLevel
                                          , lexLevelNATF =
                                               dssi' ^. dssiNATFLexicalLevel
                                          }
        put st { _lexConfig = lexConfig' }
        return $ RecordDSID dsid'
      DS -> do
        st <- get
        let dspm' = (fromS57FileDataRecord $ dr) :: DSPM
            lexConfig' = (st ^. lexConfig){ lexLevelCoordinateMulFactor =
                                               dspm' ^. dspmCoordinateMulFactor,
                                            lexLevelSoundingMulFactor =
                                              dspm' ^. dspmSoundingMulFactor
                                          }
        put st { _lexConfig = lexConfig' }
        return $ RecordDSPM dspm'
      IsolatedNode -> get >>= return . readVRID 
      ConnectedNode -> get >>= return . readVRID 
      Edge -> get >>= return . readVRID 
      Face -> get >>= return . readVRID 
      FE -> return . RecordFRID . fromS57FileDataRecord $ dr

   
s57FileSource :: (MonadResource m, MonadThrow m) =>
                 FilePath -> Producer m S57Record
s57FileSource fp = toProducer $ (CB.sourceFile fp  $= s57Conduit)
