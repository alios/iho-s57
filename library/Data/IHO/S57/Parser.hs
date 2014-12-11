{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.IHO.S57.Parser
        where

import Prelude hiding (take)
import Data.Monoid
import Control.Lens
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word
import Data.Int
import Data.Binary.Get (runGet, getWord16le, getWord32le)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (ord)
import Data.Tree
import Data.IHO.S57.Types

data DataStructCode = Empty | Linear | MultiDim deriving (Show, Eq)
data DataTypeCode = CharData | ImplicitPoint | Binary | MixedDataTypes deriving (Show, Eq)

type FieldInfo = (Text, DataStructCode, DataTypeCode, [Text], [Int -> Parser S57Value])

data DDR = DDR {
  _ddrDesc :: Text,
  _ddrFieldControlField :: [(Text, Text)],
  _ddrFieldInfo :: Map Text FieldInfo
  } 
makeLenses ''DDR

data LexLevelConfig =
  LexLevelConfig {
    lexLevelDefault :: Int,
    lexLevelATTF :: Int,
    lexLevelNATF :: Int
    }

defaultLexLevelConfig = LexLevelConfig 0 1 1

parseS57File :: Parser S57File
parseS57File = do
  rs <- parseS57File'
  return $ fmap dropISO rs

parseS57File' :: Parser S57File
parseS57File' = do
  ddr <- parseDDR <?> "ddr record"
  dr0' <- parseDR ddr defaultLexLevelConfig <?> "data record 0"
  
  let _dsid = dropISO $ readDRs ddr dr0'
      _dssi = lookupChildField "DSID" _dsid "DSSI"
  let lexLevelConfig =
        case (structureFieldName $ rootLabel _dsid) of
         "DSID" ->
           let r = structureLinearField $ rootLabel _dssi
               lookupF k = fromS57Value $
                           maybe (error $ "DSSI: unable to lookup key " ++ T.unpack k)
                           id $ Map.lookup k r
           in defaultLexLevelConfig { lexLevelATTF = lookupF "AALL"
                                    , lexLevelNATF = lookupF "NALL"
                                    }
         _ -> defaultLexLevelConfig
  drs <- (parseDR ddr lexLevelConfig) `manyTill` endOfInput
  return $ fmap (readDRs ddr) (dr0':drs)





ddrLookup' fn ddr = do
  Map.lookup fn $ ddr ^. ddrFieldInfo

ddrLookup fn ddr  = maybe (error $ "unable to lookup field info in DDR for: " ++ show fn) id $
                    ddrLookup' fn ddr
   


ddrLookupChildren fn ddr =
    let fcf = ddr ^. ddrFieldControlField
    in fmap snd $ filter (\(p, _) -> fn == p) fcf
       
ddrLookupParent fn ddr =
  let fcf = ddr ^. ddrFieldControlField
  in case (filter (\(_, c) -> fn == c) fcf) of
      [] -> Nothing
      ((p, _):[]) -> Just p
      _ -> error "ddrLookupParent: found more then one parent"



filterFieldsByName rs fn = filter (\r -> (structureFieldName r) == fn) rs



readDRs _ [] = error "readDRs: empty structure"
readDRs _ (_:[]) = error "readDRs: structure has only one record"
readDRs ddr (dr0:dr1:drs)
  | (structureFieldName dr0 /= "0001") = error "readDRs: first record must be 0001"
  | otherwise = Node {
      rootLabel = dr0,
      subForest = [
        Node {
           rootLabel = dr1,
           subForest = buildSubForrest ddr dr1 drs
           }
        ]
      }
  
buildSubForrest ddr dr1 drs =
  let cns = ddrLookupChildren (structureFieldName dr1) ddr
      cns' = mconcat $ fmap (filterFieldsByName drs) cns
      emptyTree l = Node l []
  in fmap emptyTree cns'


parseFT = char '\RS'
parseUT = char '\US'
tagL = 4







parseDDR = do
  (baseAddr, lengthL, posL) <- parseDDRLeader
  dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
  (fcfRaw:fsRaw) <- parseDirectory dir
  let fcf = doParseFieldControlField  $ snd fcfRaw
      fs = fmap doParseDDField fsRaw
  return $ DDR {
    _ddrDesc = T.pack $ fst fcf,
    _ddrFieldControlField = snd fcf,
    _ddrFieldInfo = Map.fromList $ fs
    }


parseDRs ddr ll = do
  done <- atEnd
  if (done) then return []
    else do dr <- parseDR ddr ll
            drs <- parseDRs ddr ll
            return (dr:drs)

parseDR ddr ll = do
  (baseAddr, lengthL, posL) <- parseDRLeader
  dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
  fs <- parseDirectory dir
  return (fmap (parseDRField ll ddr) fs)



lookupLexLevel ll fn =
  if (fn == "*ATTF") then lexLevelATTF ll
  else if (fn == "*NATF") then lexLevelNATF ll
       else lexLevelDefault ll               

  
parseDRField ll ddr (fn, bs) =
  let (_,structCode,typeCode,ad,ps) = ddrLookup fn ddr
      ll' = lookupLexLevel ll fn
  in case (structCode) of
      Empty ->  S57SingleValue fn $ either error id $ parseOnly ((head ps) ll') bs
      Linear -> S57LinearValue fn $ either error id $ parseOnly (parseLinear ll ad ps) bs
      MultiDim -> S57MultiValue fn $ either error id $ parseOnly (parseMultiDim ll ad ps) bs


parseMultiDim :: LexLevelConfig -> [Text] -> [Int -> Parser S57Value] -> Parser [Map Text S57Value]
parseMultiDim ll fns pss = (parseLinear ll fns pss) `manyTill` parseFT

parseLinear :: LexLevelConfig -> [Text] -> [Int -> Parser S57Value] -> Parser (Map Text S57Value)
parseLinear _ [] [] = return mempty
parseLinear ll (fn:fns) (p:ps) =
  let ll' = lookupLexLevel ll fn
  in do
    f <- p ll'
    rs <- parseLinear ll fns ps
    return $ Map.insert fn f rs




doParseFieldControlField bs =
  case (parseOnly (parseFieldControlField) bs) of
   Left err -> error $ "doParseFieldControlField: " ++ show err ++ " on " ++ show bs
   Right r -> r


parseFieldControlField = do
  _ <- string "0000;&"
  lexL <- parseLexLevel
  title <- anyChar `manyTill` parseUT
  let takeFieldPair = do
        a <- parseRLString lexL tagL
        b <- parseRLString lexL tagL
        return (a,b)
  fps <- takeFieldPair `manyTill` parseFT
  return (title, fps)


doParseDDField (tag, bs) =
  case (parseOnly parseDDField bs) of
   Left err -> error $ "doParseDDField: (" ++ show tag ++ ") " ++ show err ++ " on " ++ show bs
   Right r -> (tag, r)


type DirectoryEntry = (String, Int, Int)

parseDirectory :: [DirectoryEntry] -> Parser [(Text, ByteString)]
parseDirectory = parseFieldsR 0

parseFieldsR :: Int -> [DirectoryEntry] -> Parser [(Text, ByteString)]
parseFieldsR _ [] = return []
parseFieldsR i (d:ds) = do
  (o, f) <- parseField' i d
  fs <- parseFieldsR o ds
  return (f:fs)
  where parseField' :: Int -> DirectoryEntry -> Parser (Int, (Text, ByteString))
        parseField' o0 (tag, o1, l) = do
          _ <- if (o1 > o0) then (do _ <- take (o1 - o0) ; return ()) else return ()
          f <- take l
          return (o1 + l, (T.pack tag, f))
  

  
parseDDField = do
  structCode <- parseDataStructCode
  typeCode <- parseDataTypeCode
  _ <- string "00;&"
  lexL <- parseLexLevel
  fieldName <- parseUTString lexL
  ad <-  parseFieldNames
  fcs <- parseFieldControl lexL
  if ((length ad /= length fcs) && (null ad && (length fcs) /= 1))
    then fail $ "parseDDField invalid field/parser data, fcs length: " ++ show (length fcs)
    else return (fieldName, structCode, typeCode, ad, fcs)


parseLexLevel =
  choice [ string "   " >> return (0 :: Int)
         , string "-A " >> return 1
         , string "%/A" >> return 2
         ]
  
parseDDRLeader = do
  recLen <- parseInt 5
  _ <- string "3LE1 09"
  baseAddr <- parseInt 5
  _ <- string " ! "
  lengthL <- parseInt 1
  posL <- parseInt 1
  _ <- string "04"  
  return (baseAddr, lengthL, posL)



parseDRLeader = do
  recLen <- parseInt 5
  _ <- string " D     "
  baseAddr <- parseInt 5
  _ <- string "   "
  lengthL <- parseInt 1
  posL <- parseInt 1
  _ <- string "04"  
  return (baseAddr, lengthL, posL)

parseDirectoryEntry tagL lengthL posL = do
  tag <- count tagL anyChar
  len <- parseInt lengthL
  pos <- parseInt posL
  return (tag, pos, len)


type TypeInfo = Maybe Int


parseFieldNames = do
  fn <- fmap T.pack $ many' (satisfy $ notInClass "!\US")
  s <- satisfy $ inClass "!\US"
  fns <- if (s == '!') then parseFieldNames else return []
  return $ if (T.null fn) then fns else (fn:fns)

parseFieldControl ll = do
  _ <- char '('
  is <- parseS57Values
  _ <- parseFT
  return is


parseS57Values :: Parser [Int -> Parser S57Value]
parseS57Values = do
  v <- parseS57Value
  s <- choice [ char ',', char ')' ]
  vs <- if (s == ',') then parseS57Values else return []
  return $ v ++ vs



xx = fmap (fmap $ charDataParser) $ parseType' "A"

parseS57Value :: Parser [Int -> Parser S57Value]
parseS57Value =
  
  choice [ fmap (fmap $ charDataParser) $ parseType' "A"
         , fmap (fmap $ intDataParser) $ parseType' "I"
         , fmap (fmap $ realDataParser) $ parseType' "R"
         , fmap (fmap $ bitsDataParser) $ parseType' "B"
         , fmap (fmap $ unsignedDataParser 1) $ parseType' "b11"
         , fmap (fmap $ unsignedDataParser 2) $ parseType' "b12"
         , fmap (fmap $ unsignedDataParser 4) $ parseType' "b14"
         , fmap (fmap $ signedDataParser 1) $ parseType' "b21"
         , fmap (fmap $ signedDataParser 2) $ parseType' "b22"
         , fmap (fmap $ signedDataParser 4) $ parseType' "b24"                 
         ]



parseType' :: ByteString -> Parser [TypeInfo]
parseType' tc = do
  ds <- many' digit
  let c = if (null ds) then 1 else read ds
  _ <- string tc
  arg <- choice [ do _ <- char '('; a <- fmap (Just . read ) $
                                         digit `manyTill` char ')'; return a
                , return Nothing
                ]
  return . replicate c $ (arg)

parseRLString ll rl = fmap fromS57Value $ charDataParser (Just rl) ll
parseUTString ll = fmap fromS57Value $ charDataParser Nothing ll


dataParser c arg ll =
  fmap c $ case arg of
   Nothing -> (lexLevelParser ll) `manyTill` parseUT
   Just n  -> count n (lexLevelParser ll)
   
charDataParser = dataParser (S57CharData .  mconcat)
intDataParser = dataParser (S57Int . readParserDefault 0)

readParserDefault t [] = t
readParserDefault _ cs = read . T.unpack . mconcat $ cs


realDataParser = dataParser (S57Real . readParserDefault 0.0)

bitsDataParser (Just n) _ =
  let bytesToRead = dN + if (mN /= 0) then 1 else 0
      dN = n `div` 8
      mN = n `mod` 8
  in fmap (S57Bits) $ take bytesToRead
btisDataParser arg _ = fail $ "bitsDataParser: undefined arg " ++ show arg 

singedDataParser w arg _ = error "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
signedDataParser w Nothing _ =
  let p = case (w) of
        1 -> fmap (fromInteger . toInteger) parseInt8
        2 -> fmap (fromInteger . toInteger) parseInt16
        4 -> fmap (fromInteger . toInteger) parseInt32
  in  fmap S57Int p

unsingedDataParser w arg _ = error "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
unsignedDataParser w Nothing _ =
  let p = case (w) of
        1 -> fmap (fromInteger . toInteger) parseWord8
        2 -> fmap (fromInteger . toInteger) parseWord16
        4 -> fmap (fromInteger . toInteger) parseWord32
  in  fmap S57Int p


parseInt :: Int -> Parser Int
parseInt i = (fmap read $ count i anyChar) <?> "parseInt"

parseString = string . BL.toStrict . C.pack

parseInt8 :: Parser Int8
parseInt8 = fmap fromIntegral parseWord8
parseInt16 :: Parser Int16
parseInt16 = fmap fromIntegral parseWord16
parseInt32 :: Parser Int32
parseInt32 = fmap fromIntegral parseWord32

parseWord8 :: Parser Word8
parseWord8 = fmap (head . BS.unpack) $ take 1
parseWord16 :: Parser Word16
parseWord16 = fmap (runGet getWord16le . BL.fromStrict) $ take 2
parseWord32 :: Parser Word32
parseWord32 = fmap (runGet getWord32le . BL.fromStrict) $ take 4

lexLevelParser 0 = fmap T.singleton $ satisfy isAscii
lexLevelParser 1 = fmap T.singleton $ satisfy isISO8859_1
lexLevelParser 2 = fmap T.decodeUtf16LE $ take 2

isAscii :: Char -> Bool
isAscii c = (ord c) < 128

isISO8859_1 :: Char -> Bool
isISO8859_1 c = (ord c) < 256


parseDataStructCode = do
  c <- satisfy $ inClass "012"
  case c of
   '0' -> return Empty
   '1' -> return Linear
   '2' -> return MultiDim
   c' -> fail $ "parseDataStructCode: undefined code: " ++ show c'



parseDataTypeCode = do
  c <- satisfy $ inClass "0156"
  case c of
   '0' -> return CharData
   '1' -> return ImplicitPoint
   '5' -> return Binary
   '6' -> return MixedDataTypes
   c' -> fail $ "parseDataTypeCode: undefined code: " ++ show c'


s57 :: ()
s57 = ()

