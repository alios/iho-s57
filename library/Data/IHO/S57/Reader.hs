{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.IHO.S57.Reader where
import Control.Lens
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Control.Monad.RWS
import Data.Conduit
import Data.Conduit.Lift
import Data.Conduit.Attoparsec
import Data.Conduit.Binary
import Data.Tree (Tree)

import Data.IHO.S57.Types
import Data.IHO.S57.Parser
import Data.IHO.S57.DSID
import Data.IHO.S57.DSPM
import Data.IHO.S57.FRID
import Data.IHO.S57.VRID    


data ReaderState =
  ReaderState { _ddr :: Maybe DDR
              , _lexConfig :: LexLevelConfig
              } 
makeLenses ''ReaderState

ddrSink :: (MonadThrow m) => Consumer ByteString m DDR                       
ddrSink = sinkParser parseDDR

drSink :: (MonadThrow m) =>
           DDR -> LexLevelConfig -> Consumer ByteString m [S57Structure]
drSink _ddr ll = sinkParser $ parseDR _ddr ll


data S57Record =
  RecordDSID DSID |
  RecordDSPM DSPM |
  RecordVRID VRID |
  RecordFRID FRID 
  deriving (Show, Eq)
           
s57Conduit :: (MonadThrow m) => Conduit ByteString m S57Record
s57Conduit =
  evalStateLC (ReaderState Nothing defaultLexLevelConfig) $ s57ConduitS

s57ConduitS :: (MonadState ReaderState m, MonadThrow m) =>
               Conduit ByteString m S57Record
s57ConduitS = do
  ll <- fmap _lexConfig get
  ddrM <- fmap _ddr get
  ddrF <- case (ddrM) of
              Nothing -> do
                ddr' <- ddrSink
                modify (\st -> st { _ddr = Just ddr' })
                return ddr'
              Just ddr' -> return ddr'
  dr <- fmap (dropISO . readDRs ddrF) $ drSink ddrF ll
  r <- handleRecord dr
  yield r
  s57ConduitS


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
      CD -> fail "unexpected CATD record"
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
