
module Main (main) where

import Data.Conduit
import Data.Text (Text)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import Data.IHO.S57.Reader
import Text.Groom
import Control.Monad.Trans.Resource
import Control.Lens
import Data.Table

p :: FilePath
p = "/home/alios/Documents/IHO/data/NOAA/ENC_ROOT/"

s57src :: MonadResource m => Source m S57Record
s57src = s57FileSource $
         p ++ "US5FL62M/US5FL62M.000"

s57cat :: MonadResource m => Source m S57Record
s57cat = s57FileSource $
         p ++ "CATALOG.031"

groomRecord :: Monad m => Conduit S57Record m Text
groomRecord = do
  v <- await
  case v of
   Just r ->
     do yield $ T.pack $ groom r ++ "\n"
        groomRecord
   Nothing -> return ()

showRecord :: Monad m => Conduit S57Record m Text
showRecord = do
  v <- await
  case v of
   Just r ->
     do yield $ T.pack $ show r ++ "\n"
        groomRecord
   Nothing -> return ()

fileOutput :: (MonadThrow m, MonadResource m) => FilePath -> Sink Text m ()
fileOutput fp = CT.encode CT.utf8 =$ CB.sinkFile fp 

readDF :: IO ()
readDF = do
  ds <- runResourceT $ s57src $$ s57readDataSet
  let vs = (ds ^. dataSetVRIDTable ^. from table)
  let fs = (ds ^. dataSetFRIDTable ^. from table)
  _ <- sequence $ fmap (putStrLn . groom) vs
  _ <- sequence $ fmap (putStrLn . groom) fs
  return ()

readCat :: IO ()
readCat = do
  ds <- runResourceT $ s57cat $$ s57readCatalog
  let (S57Catalog catds) = ds
  _ <- sequence $ fmap (putStrLn . groom) catds
  return ()

  
main :: IO ()
main = do
  readCat
  return ()
