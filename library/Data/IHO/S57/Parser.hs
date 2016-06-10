{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.IHO.S57.Parser
       ( DDR(..), ddrDesc, ddrFileControlField, ddrFieldInfo
       , parseDDR
       , parseDR, readDRs
       , LexLevelConfig(..), defaultLexLevelConfig
       ) where

import           Control.Lens.Getter
import           Control.Lens.TH
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary.Get                  (getWord16le, getWord32le,
                                                   runGet)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (ord)
import           Data.IHO.S57.Types
import           Data.Int
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Data.Tree
import           Data.Word
import           Prelude                          hiding (take)

data DataStructCode = Empty | Linear | MultiDim deriving (Show, Eq)
data DataTypeCode = CharData | ImplicitPoint | Binary | MixedDataTypes deriving (Show, Eq)

type FieldInfo = (Text, DataStructCode, DataTypeCode, [Text], [Int -> Parser S57Value])

data DDR = DDR {
  _ddrDesc             :: Text,
  _ddrFileControlField :: [(Text, Text)],
  _ddrFieldInfo        :: Map Text FieldInfo
  }
makeLenses ''DDR

data LexLevelConfig =
  LexLevelConfig {
    lexLevelDefault             :: Int,
    lexLevelATTF                :: Int,
    lexLevelNATF                :: Int,
    lexLevelCoordinateMulFactor :: Int,
    lexLevelSoundingMulFactor   :: Int
    }

defaultLexLevelConfig :: LexLevelConfig
defaultLexLevelConfig = LexLevelConfig {
  lexLevelDefault = 1,
  lexLevelATTF = error "ATTF lex level not set",
  lexLevelNATF = error "NATF lex level not set",
  lexLevelCoordinateMulFactor = 1,
  lexLevelSoundingMulFactor = 1
  }

ddrLookup' :: Text -> DDR -> Maybe FieldInfo
ddrLookup' fn ddr = do
  Map.lookup fn $ ddr ^. ddrFieldInfo


ddrLookup :: Text -> DDR -> FieldInfo
ddrLookup fn ddr  =
  maybe (error $ "unable to lookup field info in DDR for: " ++ show fn) id $
  ddrLookup' fn ddr


ddrLookupChildren :: Text -> DDR -> [Text]
ddrLookupChildren fn ddr =
    let fcf = ddr ^. ddrFileControlField
    in fmap snd $ filter (\(p, _) -> fn == p) fcf

filterFieldsByName :: [S57Structure] -> Text -> [S57Structure]
filterFieldsByName rs fn = filter (\r -> (structureFieldName r) == fn) rs


readDRs :: DDR -> [S57Structure] -> Tree S57Structure
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

buildSubForrest :: DDR -> S57Structure -> [S57Structure] -> [Tree S57Structure]
buildSubForrest ddr dr1 drs =
  let cns = ddrLookupChildren (structureFieldName dr1) ddr
      cns' = mconcat $ fmap (filterFieldsByName drs) cns
      emptyTree l = Node l []
  in fmap emptyTree cns'

parseFT, parseUT :: Parser Char
parseFT = char '\RS'
parseUT = char '\US'
tagL :: Int
tagL = 4


parseDDR :: Parser DDR
parseDDR = do
  (_, lengthL, posL) <- parseDDRLeader
  dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
  (fcfRaw:fsRaw) <- parseDirectory dir
  let fcf = doParseFileControlField  $ snd fcfRaw
      fs = fmap doParseDDField fsRaw
  return $ DDR {
    _ddrDesc = T.pack $ fst fcf,
    _ddrFileControlField = snd fcf,
    _ddrFieldInfo = Map.fromList $ fs
    }

parseDR :: DDR -> LexLevelConfig -> Parser (Maybe [S57Structure])
parseDR ddr ll = do
  done <- atEnd
  if (done) then return Nothing
    else do
    (_, lengthL, posL) <- parseDRLeader
    dir <- (parseDirectoryEntry tagL lengthL posL) `manyTill` parseFT
    fs <- parseDirectory dir
    return . Just $ (fmap (parseDRField ll ddr) fs)

lookupLexLevel :: LexLevelConfig -> Text -> Int
lookupLexLevel ll fn =
  if (fn == "*ATTF") then lexLevelATTF ll
  else if (fn == "*NATF") then lexLevelNATF ll
       else lexLevelDefault ll

parseDRField :: LexLevelConfig -> DDR -> (Text, ByteString) -> S57Structure
parseDRField ll ddr (fn, bs) =
  let (_,structCode,_,ad,ps) = ddrLookup fn ddr
      ll' = lookupLexLevel ll fn
  in case (structCode) of
      Empty ->  S57SingleValue fn $ either error id $ parseOnly ((head ps) ll') bs
      Linear -> S57LinearValue fn $ either error id $ parseOnly (parseLinear ll ad ps) bs
      MultiDim -> S57MultiValue fn $ either error id $ parseOnly (parseMultiDim ll ad ps) bs


parseMultiDim :: LexLevelConfig -> [Text] -> [Int -> Parser S57Value] -> Parser [Map Text S57Value]
parseMultiDim ll fns pss = (parseLinear ll fns pss) `manyTill` parseFT

parseLinear :: LexLevelConfig -> [Text] -> [Int -> Parser S57Value] -> Parser (Map Text S57Value)
parseLinear _ [] _ = return mempty
parseLinear _ _ [] = return mempty
parseLinear ll (fn:fns) (p:ps) =
  let ll' = lookupLexLevel ll fn
  in do
    f <- p ll'
    rs <- parseLinear ll fns ps
    return $ Map.insert fn f rs



doParseFileControlField :: ByteString -> (String, [(Text, Text)])
doParseFileControlField bs =
  case (parseOnly (parseFileControlField) bs) of
   Left err -> error $ "doParseFileControlField: " ++ show err ++ " on " ++ show bs
   Right r -> r

parseFileControlField :: Parser (String, [(Text, Text)])
parseFileControlField = do
  _ <- string "0000;&"
  lexL <- parseLexLevel
  title <- anyChar `manyTill` parseUT
  let takeFieldPair = do
        a <- parseRLString lexL tagL
        b <- parseRLString lexL tagL
        return (a,b)
  fps <- takeFieldPair `manyTill` parseFT
  return (title, fps)


type DDField =
  (Text, DataStructCode, DataTypeCode, [Text], [Int -> Parser S57Value])

doParseDDField :: Show t => (t, ByteString) -> (t, DDField)
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


parseDDField :: Parser DDField
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

parseLexLevel :: Parser Int
parseLexLevel =
  choice [ string "   " >> return (0 :: Int)
         , string "-A " >> return 1
         , string "%/A" >> return 2
         ]

parseDDRLeader :: Parser (Int, Int, Int)
parseDDRLeader = do
  _ <- parseInt 5 -- recLen
  _ <- string "3LE1 09"
  baseAddr <- parseInt 5
  _ <- choice [string " ! "
              ,string "   "]
  lengthL <- parseInt 1
  posL <- parseInt 1
  _ <- string "04"
  return (baseAddr, lengthL, posL)


parseDRLeader :: Parser (Int, Int, Int)
parseDRLeader = do
  _ <- parseInt 5 -- recLen
  _ <- string " D     "
  baseAddr <- parseInt 5
  _ <- string "   "
  lengthL <- parseInt 1
  posL <- parseInt 1
  _ <- string "04"
  return (baseAddr, lengthL, posL)

parseDirectoryEntry :: Int -> Int -> Int -> Parser (String, Int, Int)
parseDirectoryEntry _tagL lengthL posL = do
  tag <- count _tagL anyChar
  len <- parseInt lengthL
  pos <- parseInt posL
  return (tag, pos, len)


type TypeInfo = Maybe Int

parseFieldNames :: Parser [Text]
parseFieldNames = do
  fn <- fmap T.pack $ many' (satisfy $ notInClass "!\US")
  s <- satisfy $ inClass "!\US"
  fns <- if (s == '!') then parseFieldNames else return []
  return $ if (T.null fn) then fns else (fn:fns)

parseFieldControl :: Int -> Parser [Int -> Parser S57Value]
parseFieldControl _ = do
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

parseRLString :: FromS57Value b => Int -> Int -> Parser b
parseRLString ll rl = fmap fromS57Value $ charDataParser (Just rl) ll
parseUTString :: FromS57Value b => Int -> Parser b
parseUTString ll = fmap fromS57Value $ charDataParser Nothing ll

dataParser :: ([Text] -> b) -> Maybe Int -> Int -> Parser b
dataParser c arg ll =
  fmap c $ case arg of
   Nothing -> (lexLevelParser ll) `manyTill` parseUT
   Just n  -> count n (lexLevelParser ll)


charDataParser :: TypeInfo -> Int -> Parser S57Value
charDataParser = dataParser (S57CharData .  mconcat)
intDataParser :: TypeInfo -> Int -> Parser S57Value
intDataParser = dataParser (S57Int . readParserDefault 0)

readParserDefault :: (Read t) => t -> [Text] -> t
readParserDefault t [] = t
readParserDefault _ cs = read . T.unpack . mconcat $ cs

realDataParser :: TypeInfo -> Int -> Parser S57Value
realDataParser = dataParser (S57Real . readParserDefault 0.0)

bitsDataParser :: TypeInfo -> Int -> Parser S57Value
bitsDataParser (Just n) _ =
  let bytesToRead = dN + if (mN /= 0) then 1 else 0
      dN = n `div` 8
      mN = n `mod` 8
  in fmap (S57Bits) $ take bytesToRead
bitsDataParser arg _ = fail $ "bitsDataParser: undefined arg " ++ show arg


signedDataParser :: Int -> TypeInfo -> Int -> Parser S57Value
signedDataParser w (Just arg) _  =
  error $ "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
signedDataParser w Nothing _ =
  let p = case (w) of
        1 -> fmap (fromInteger . toInteger) parseInt8
        2 -> fmap (fromInteger . toInteger) parseInt16
        4 -> fmap (fromInteger . toInteger) parseInt32
        n -> error $ "signedDataParser: undefined width " ++ show n
  in  fmap S57Int p

unsignedDataParser :: Int -> TypeInfo -> Int -> Parser S57Value
unsignedDataParser w (Just arg) _ =
  error $ "signedDataParser: w: " ++ show w ++ " arg: " ++ show arg
unsignedDataParser w Nothing _ =
  let p = case (w) of
        1 -> fmap (fromInteger . toInteger) parseWord8
        2 -> fmap (fromInteger . toInteger) parseWord16
        4 -> fmap (fromInteger . toInteger) parseWord32
        n -> error $ "unsignedDataParser: undefined width " ++ show n
  in  fmap S57Int p


parseInt :: Int -> Parser Int
parseInt i = (fmap read $ count i anyChar) <?> "parseInt"

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

lexLevelParser :: Int -> Parser Text
lexLevelParser 0 = fmap T.singleton $ satisfy isAscii
lexLevelParser 1 = fmap T.singleton $ satisfy isISO8859_1
lexLevelParser 2 = fmap T.decodeUtf16LE $ take 2
lexLevelParser ll = error $ "lexLevelParser: undefined LexLevel " ++ show ll

isAscii :: Char -> Bool
isAscii c = (ord c) < 128

isISO8859_1 :: Char -> Bool
isISO8859_1 c = (ord c) < 256

parseDataStructCode :: Parser DataStructCode
parseDataStructCode = do
  c <- satisfy $ inClass "012"
  case c of
   '0' -> return Empty
   '1' -> return Linear
   '2' -> return MultiDim
   c' -> fail $ "parseDataStructCode: undefined code: " ++ show c'

parseDataTypeCode :: Parser DataTypeCode
parseDataTypeCode = do
  c <- satisfy $ inClass "0156"
  case c of
   '0' -> return CharData
   '1' -> return ImplicitPoint
   '5' -> return Binary
   '6' -> return MixedDataTypes
   c' -> fail $ "parseDataTypeCode: undefined code: " ++ show c'
