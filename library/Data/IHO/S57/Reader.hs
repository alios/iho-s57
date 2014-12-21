{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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



s57ConduitState :: (MonadState ReaderState m, MonadThrow m) =>
                   Conduit ByteString m (S57FileRecord)
s57ConduitState = do
  ll <- fmap _lexConfig get
  ddrM <- fmap _ddr get
  ddrF <- case (ddrM) of
              Nothing -> do
                ddr' <- ddrSink
                modify (\st -> st { _ddr = Just ddr' })
                return ddr'
              Just ddr' -> return ddr'
  dr <- fmap (readDRs ddrF) $ drSink ddrF ll
  handleRecord dr
  yield dr
  s57ConduitState


handleRecord :: (MonadState ReaderState m) => S57FileRecord -> m ()
handleRecord dr =
  let rn = readRecordName dr
  in case (rn ^. rcnm) of
      CD -> fail "unexpected CATD record"
      DS -> do
        st <- get
        let dsid' = fromS57FileDataRecord $ dr
            dssi' = dsid' ^. dsidDSSI
            lexConfig' = (st ^. lexConfig){ lexLevelATTF =
                                               dssi' ^. dssiATTFLexicalLevel
                                          , lexLevelNATF =
                                               dssi' ^. dssiNATFLexicalLevel
                                          }
        put st { _lexConfig = lexConfig' }
      DP -> do
        st <- get
        let dspm' = fromS57FileDataRecord $ dr
            lexConfig' = (st ^. lexConfig){ lexLevelCoordinateMulFactor =
                                               dspm' ^. dspmCoordinateMulFactor,
                                            lexLevelSoundingMulFactor =
                                              dspm' ^. dspmSoundingMulFactor
                                          }
        put st { _lexConfig = lexConfig' }
      _ -> return ()
