
module Main (main) where

import Data.Conduit
import Data.Text (Text)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import qualified Data.Text as T
import Data.IHO.S57.Reader
import Text.Groom
import Control.Monad.Trans.Resource


s57src :: MonadResource m => Source m S57Record
s57src = s57FileSource
         "/home/alios/Documents/IHO/data/NOAA/ENC_ROOT/US5FL62M/US5FL62M.000"

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

main :: IO ()
main = runResourceT $ 
  s57src $= showRecord $$ fileOutput "/tmp/foo.txt"
