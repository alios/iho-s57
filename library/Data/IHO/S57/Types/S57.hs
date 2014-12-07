{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.IHO.S57.Types.S57 (s57) where

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


data S57Value =
  S57CharData !Text |
  S57Int !Int |
  S57Real !Double |
  S57Bits !ByteString 
  deriving (Eq, Show)

data S57Structure =
  S57SingleValue Text S57Value |
  S57LinearValue Text [(Text, S57Value)] |
  S57MultiValue  Text [[(Text, S57Value)]]
  deriving (Show, Eq)

           
data DataStructCode = Empty | Linear | MultiDim deriving (Show, Eq)
data DataTypeCode = CharData | ImplicitPoint | Binary | MixedDataTypes deriving (Show, Eq)

type FieldInfo = (Text, DataStructCode, DataTypeCode, [Text], [Parser S57Value])

data DDR = DDR {
  _ddrDesc :: Text,
  _ddrFieldControlField :: Map Text Text,
  _ddrFieldInfo :: Map Text FieldInfo
  } 
makeLenses ''DDR


ddrLookup' fn ddr = do
  Map.lookup fn $ ddr ^. ddrFieldInfo

ddrLookup fn ddr  = maybe (error $ "unable to lookup field info in DDR for: " ++ show fn) id $
                    ddrLookup' fn ddr
   



parseFT = char '\RS'
parseUT = char '\US'
tagL = 4



parseFile = do
  ddr <- parseDDR
  drs <- parseDR ddr `manyTill` endOfInput
  return (drs)

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






parseDDR = do
  (baseAddr, lengthL, posL) <- parseDDRLeader
  dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
  (fcfRaw:fsRaw) <- parseDirectory dir
  let fcf = doParseFieldControlField $ snd fcfRaw
      fs = fmap doParseDDField fsRaw
  return $ DDR {
    _ddrDesc = T.pack $ fst fcf,
    _ddrFieldControlField = snd fcf,
    _ddrFieldInfo = Map.fromList $ fs
    }


parseDRs ddr = do
  done <- atEnd
  if (done) then return []
    else do dr <- parseDR ddr
            drs <- parseDRs ddr
            return (dr:drs)

parseDR ddr = do
  (baseAddr, lengthL, posL) <- parseDRLeader
  dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
  fs <- parseDirectory dir
  return (fmap (parseDRField ddr) fs)


--parseDRField :: DDR -> (Text, ByteString) -> (Text, ByteString)
parseDRField ddr (fn, bs) =
  let (_,structCode,typeCode,ad,ps) = ddrLookup fn ddr
  in case (structCode) of
      Empty ->  S57SingleValue fn $ either error id $ parseOnly (head ps) bs
      Linear -> S57LinearValue fn $ either error id $ parseOnly (parseLinear ad ps) bs
     
parseLinear :: [Text] -> [Parser S57Value] -> Parser [(Text, S57Value)]
parseLinear [] [] = return []
parseLinear (fn:fns) (p:ps) = do
  f <- p
  rs <- parseLinear fns ps
  return $ (fn, f):rs




doParseFieldControlField bs =
  case (parseOnly parseFieldControlField bs) of
   Left err -> error $ "doParseFieldControlField: " ++ show err ++ " on " ++ show bs
   Right r -> r

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
  
parseFieldControlField = do
  _ <- string "0000;&"
  lexL <- parseLexLevel
  title <- anyChar `manyTill` parseUT
  let takeFieldPair = do
        a <- parseRLString 0 tagL
        b <- parseRLString 0 tagL
        return (a,b)
  fps <- takeFieldPair `manyTill` parseFT
  return (title, Map.fromList fps)

  
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
  choice [ string "   " >> return 0
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

fromS57CharData (S57CharData t) = t
fromS57CharData t = error $ "fromS57CharData: is not char data: " ++ show t



parseFieldNames = do
  fn <- fmap T.pack $ many' (satisfy $ notInClass "!\US")
  s <- satisfy $ inClass "!\US"
  fns <- if (s == '!') then parseFieldNames else return []
  return $ if (T.null fn) then fns else (fn:fns)

parseFieldControl ll = do
  _ <- char '('
  is <- parseS57Values ll
  _ <- parseFT
  return is


parseS57Values :: Int -> Parser [Parser S57Value]
parseS57Values ll = do
  v <- parseS57Value ll
  s <- choice [ char ',', char ')' ]
  vs <- if (s == ',') then parseS57Values ll else return []
  return $ v ++ vs
  
parseS57Value :: Int -> Parser [Parser S57Value]
parseS57Value ll =
  choice [ fmap (fmap $ charDataParser ll) $ parseType' "A"
         , fmap (fmap $ intDataParser  ll) $ parseType' "I"
         , fmap (fmap $ realDataParser  ll) $ parseType' "R"
         , fmap (fmap $ bitsDataParser) $ parseType' "B"
         , fmap (fmap $ unsignedDataParser 1) $ parseType' "b11"
         , fmap (fmap $ unsignedDataParser 2) $ parseType' "b12"
         , fmap (fmap $ unsignedDataParser 4) $ parseType' "b14"
         , fmap (fmap $ signedDataParser 1) $ parseType' "b21"
         , fmap (fmap $ signedDataParser 2) $ parseType' "b22"
         , fmap (fmap $ signedDataParser 4) $ parseType' "b24"                 
         ]


tx = ["A", "A(2)", "A()", "2A", "2A(2)", "2A()","B"]
xx = fmap (parseOnly (parseType' "A")) tx

ff :: Double
ff = read "-32.566666680000"

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





{-

I(5)  00002\RS

A(2): CD
I(10) 0000000002
A     GB5X01SW.001\US
A     \US
A     V01X01\US
A(3)  BIN
R     -32.566666680000\US
R     60.866666680000\US
R     -32.500000000000\US
R     60.966666680000\US
A     055C2ACA\US
A     \US
      \RS

-}
{-
(A(2),I(10),3A,A(3),4R,2A)

A(2),
I(10),
A,
A,
A,
A(3),
R,
R,
R,
R,
A,
A)



-}

parseRLString ll rl = fmap fromS57CharData $ charDataParser ll (Just rl)
parseUTString ll = fmap fromS57CharData $ charDataParser ll Nothing


dataParser c ll arg =
  fmap c $ case arg of
   Nothing -> (lexLevelParser ll) `manyTill` parseUT
   Just n  -> count n (lexLevelParser ll)
   
charDataParser = dataParser (S57CharData .  mconcat)
intDataParser = dataParser (S57Int . readParserDefault 0)

readParserDefault t [] = t
readParserDefault _ cs = read . T.unpack . mconcat $ cs


realDataParser = dataParser (S57Real . readParserDefault 0.0)


bitsDataParser :: TypeInfo -> Parser S57Value
bitsDataParser (Just n) = fmap (S57Bits) $ take n -- TODO: /8
btisDataParser arg = fail $ "bitsDataParser: undefined arg " ++ show arg 


signedDataParser :: Int -> TypeInfo -> Parser S57Value
singedDataParser w arg = error "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
signedDataParser w Nothing =
  let p = case (w) of
        1 -> fmap (fromInteger . toInteger) parseInt8
        2 -> fmap (fromInteger . toInteger) parseInt16
        4 -> fmap (fromInteger . toInteger) parseInt32
  in  fmap S57Int p

unsignedDataParser :: Int -> TypeInfo -> Parser S57Value
unsingedDataParser w arg = error "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
unsignedDataParser w Nothing =
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


s57 :: ()
s57 = ()








td = "/home/alios/src/iho-s57/data/ENC3.1.1_TDS_Unencrypted/6.8.15.1a Receipt-Installation and Application of Updates/001/"
tf = td ++ "ENC_ROOT/CATALOG.031"
tf2 = td ++ "ENC_ROOT/GB5X01SW.001"
main = do
  bs <- BS.readFile tf2
  print $ parseOnly parseFile bs