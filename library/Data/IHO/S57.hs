{-# LANGUAGE OverloadedStrings #-}

module Data.IHO.S57 where

import Data.IHO.S57.Types
import Data.IHO.S57.Parser
import Data.IHO.S57.CATD
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import qualified Data.ByteString as BS
--import Text.Groom

parseCatalogFile :: Parser [CATD]
parseCatalogFile = fmap (fmap fromS57FileRecord) $ parseS57File


readCatalogFileIO :: FilePath -> IO [CATD]
readCatalogFileIO fn = do
  res <- fmap (parseOnly parseCatalogFile) $ BS.readFile fn
  case res of
   Left err -> fail $ "readCatalogFileIO: unable to read file '" ++ show fn ++ "': " ++ show err
   Right r -> return r

(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b 

readS57Dir :: FilePath -> IO [CATD]
readS57Dir fn =
  let enc_root = "ENC_ROOT"
      catalog = fn </> enc_root </> "CATALOG.031"
  in readCatalogFileIO catalog



td = "/home/alios/src/iho-s57/data/ENC3.1.1_TDS_Unencrypted/6.8.15.1a Receipt-Installation and Application of Updates/002/"
tf = td ++ "ENC_ROOT/CATALOG.031"
tf2 = td ++ "ENC_ROOT/GB5X01SW.001"

main = do
  catds <- readS57Dir td
  putStrLn . show $ catds
