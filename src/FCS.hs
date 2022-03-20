{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module FCS
    ( readFCS
    ) where

import Data.Time as Time
import qualified Data.ByteString.Lazy as BL
import Data.Int ( Int32, Int64 )
import Data.Binary.Get (Get, getByteString, skip, lookAhead, Decoder(Fail), runGet)
import qualified ASCII
import qualified ASCII.Char as AC
import Text.Read (readMaybe)
import Data.Void (Void)
import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Data.Char (isAscii)
import Debug.Trace ( trace )
import GHC.IO.Exception (IOException(ioe_filename))

data Version = Version
    { major :: !Int32
    , minor :: !Int32
    } deriving (Show)

getVersion :: Get Version
getVersion = getAsciiString 6 >>= \case
    [AC.CapitalLetterF, AC.CapitalLetterC, AC.CapitalLetterS, AC.Digit3, AC.FullStop, AC.Digit1] -> return $! Version 3 1
    _ -> fail "Unable to recognize FCS version"

data SegmentOffsets = SegmentOffsets
    { start :: !Int64
    , end   :: !Int64 -- _inclusive_ of the ending byte! This is the offset of the last byte (not one-past-the-end)
    } deriving (Show)

getAsciiOffset :: Get Int64
getAsciiOffset = getAsciiString 8 >>= parseAsciiOffset
    where parseAsciiOffset acs = case readMaybe $ ASCII.charListToUnicodeString acs :: Maybe Int64 of
            Nothing -> fail "Unable to decode offset"
            Just val -> return val

getSegmentOffsets :: Get SegmentOffsets
getSegmentOffsets = do
    start <- getAsciiOffset
    end   <- getAsciiOffset
    return $! SegmentOffsets start end

data Header = Header
    { version :: Version
    , textOffsets :: SegmentOffsets
    , stextOffsets :: SegmentOffsets
    , dataOffsets :: SegmentOffsets
    , analysisOffsets :: SegmentOffsets
    } deriving (Show)

getHeader :: Get Header
getHeader = Header
    <$> getVersion
    <* (getAsciiString 4 >>= (\cs -> if all (== AC.Space) cs then return True else fail "Invalid character in header spacer"))
    <*> getSegmentOffsets
    <*> return (SegmentOffsets 0 0)
    <*> getSegmentOffsets
    <*> getSegmentOffsets

data TextSegment = TextSegment
    { delimiter :: AC.Char
    , keyvals   :: Map.Map T.Text T.Text
    } deriving (Show)

getSegmentDelimiter :: T.Text -> Maybe Char
getSegmentDelimiter text = case mfirst of
                            Just first -> Just $ fst first
                            Nothing -> Nothing
                           where mfirst = T.uncons text

_splitSegmentByEscapedDelims :: Char -> T.Text -> T.Text -> [T.Text]
_splitSegmentByEscapedDelims delim pretoken text
    | secondChar == tdelim = _splitSegmentByEscapedDelims delim (T.append pretoken $ T.append token tdelim) $ T.drop 2 next
    --                     ^ We found an escaped separator. Add token to the accumulated pretoken
    | secondChar == T.empty = [T.append pretoken token]
    --                      ^ end of string reached. Return the accumulated token
    | secondChar /= tdelim = T.append pretoken token : _splitSegmentByEscapedDelims delim T.empty (T.drop 1 next)
    --                     ^ This is an actual separator, not an escaped separator. Return the accumulated token
    | otherwise = []
    --          ^ should be unreachable (but the linter complains)
                    where tdelim = T.singleton delim
                          (token, next) = T.breakOn tdelim text
                          secondChar = T.drop 1 $ T.take 2 next

splitSegmentByEscapedDelims :: Char -> T.Text -> [T.Text]
splitSegmentByEscapedDelims delim text = _splitSegmentByEscapedDelims delim T.empty trimmed
                        where tdelim = T.singleton delim
                              delimited = fst (T.breakOnEnd tdelim $ snd (T.breakOn tdelim text))
                              trimmed = T.tail $ T.init delimited

zipEveryOther :: [T.Text] -> [(T.Text, T.Text)]
zipEveryOther (x:y:rest) = (x,y) : zipEveryOther rest
zipEveryOther [x] = error "Cannot every-other zip a odd-numbered list"
zipEveryOther [] = []

getSegmentKeyvals :: Char -> T.Text -> Maybe [(T.Text, T.Text)]
getSegmentKeyvals delim text = if odd len then Nothing else Just $ zipEveryOther splits
                               where splits = splitSegmentByEscapedDelims delim text
                                     len = length splits

getTextSegment :: Int -> Get TextSegment
getTextSegment length = do
    segment <- Data.Text.Encoding.decodeUtf8 <$> getByteString length
    delimiter <- maybeGet "Text segment delimiter is invalid!" $ getSegmentDelimiter segment
    acDelimiter <- maybeGet "Delimiter is not an ASCII character" $ AC.fromIntMaybe . fromEnum $ delimiter
    keyvals <- maybeGet "Text segment has invalid key-value specification" $ getSegmentKeyvals delimiter segment
    if all (T.all isAscii . fst) keyvals then return () else fail "Specified keys are not ASCII"
    return $! TextSegment acDelimiter $ Map.fromList keyvals


updateHeader :: Header -> TextSegment -> Get Header
updateHeader initialHeader primaryText = Header
                                    (version initialHeader)
                                    (textOffsets initialHeader)
                                <$> (SegmentOffsets
                                    <$> parseKeyval (T.pack "$BEGINSTEXT") (keyvals primaryText)
                                    <*> parseKeyval (T.pack "$ENDSTEXT") (keyvals primaryText)
                                    )
                                <*> (SegmentOffsets
                                    <$> parseKeyval (T.pack "$BEGINDATA") (keyvals primaryText)
                                    <*> parseKeyval (T.pack "$ENDDATA") (keyvals primaryText)
                                    )
                                <*> (SegmentOffsets
                                    <$> parseKeyval (T.pack "$BEGINANALYSIS") (keyvals primaryText)
                                    <*> parseKeyval (T.pack "$ENDANALYSIS")  (keyvals primaryText)
                                    )


data AmplificationType = AmplificationType
    { logDecades :: Float
    , offset :: Float
    } deriving (Show)

data VisualizationScale = Linear | Logarithmic deriving (Enum, Show)
toVizScale :: T.Text -> Get VisualizationScale
toVizScale text
    | text == T.pack "Linear" = return Linear
    | text == T.pack "Logarithmic" = return Logarithmic
    | otherwise = fail "Invalid visualization scale"

data ParameterVisualizationScale = ParameterVisualizationScale
    { scale :: VisualizationScale
    , f1 :: Float
    , f2 :: Float
    } deriving (Show)

data ParameterCalibration = ParameterCalibration
    { unitConversionFactor :: Float
    , unitName :: T.Text
    } deriving (Show)

data Parameter = Parameter
    { bitLength :: Int64  -- PnB
    , amplification :: AmplificationType -- PnE
    , shortName :: T.Text -- PnN
    , range :: Int64 -- PnR
    -- Optional parameters
    , vizScale :: Maybe ParameterVisualizationScale -- PnD
    , filter :: Maybe T.Text -- PnF
    , gain :: Maybe Float -- PnG
    , excitationWavelength :: Maybe [Int64] -- PnL
    , excitationPower :: Maybe Int64 -- PnO
    , percentLightCollected :: Maybe Float -- PnP
    , name :: Maybe T.Text -- PnS
    , detectorType :: Maybe T.Text -- PnT
    , detectorVoltage :: Maybe Float -- PnV
    , calibration :: Maybe ParameterCalibration -- PnCALIBRATION
    } deriving (Show)


getParameter :: Map.Map T.Text T.Text -> Int64 -> Get Parameter
getParameter keyvalues i = Parameter
                    <$> parseKeyval (T.pack $ "$P" ++ show i ++ "B") keyvalues
                    <*> ((\[decades, offset] -> liftM2 AmplificationType
                            (maybeGet "Unable to parse amplification decades type" (readMaybeText decades))
                            (maybeGet "Unable to parse amplification offset" (readMaybeText offset))
                        ) =<< parseTokens 2 (T.singleton ',') (T.pack $ "$P" ++ show i ++ "E") keyvalues)
                    <*> maybeGet ("$P" ++ show i ++ "N key missing!") (Map.lookup (T.pack $ "$P" ++ show i ++ "N") keyvalues)
                    <*> parseKeyval (T.pack $ "$P" ++ show i ++ "R") keyvalues
                    <*> (if Map.notMember (T.pack $ "$P" ++ show i ++ "D") keyvalues then return Nothing else (\[scale, f1, f2] -> Just <$> liftM3 ParameterVisualizationScale
                            (toVizScale scale)
                            (maybeGet "Unable to parse visualization parameter 1" (readMaybeText f1))
                            (maybeGet "Unable to parse visualization parameter 2" (readMaybeText f2))
                        ) =<< parseTokens 3 (T.singleton ',') (T.pack $ "$P" ++ show i ++ "D") keyvalues)
                    <*> return (Map.lookup (T.pack $ "$P" ++ show i ++ "F") keyvalues)
                    <*> maybeParseKeyval (T.pack $ "$P" ++ show i ++ "G") keyvalues
                    <*> maybeParseList (T.singleton ',') (T.pack $ "$P" ++ show i ++ "L") keyvalues
                    <*> maybeParseKeyval (T.pack $ "$P" ++ show i ++ "O") keyvalues
                    <*> maybeParseKeyval (T.pack $ "$P" ++ show i ++ "P") keyvalues
                    <*> return (Map.lookup (T.pack $ "$P" ++ show i ++ "S") keyvalues)
                    <*> return (Map.lookup (T.pack $ "$P" ++ show i ++ "T") keyvalues)
                    <*> maybeParseKeyval (T.pack $ "$P" ++ show i ++ "F") keyvalues
                    <*> (if Map.notMember (T.pack $ "$P" ++ show i ++ "CALIBRATION") keyvalues then return Nothing else (\[cFactor, name] -> Just <$> liftM2 ParameterCalibration
                            (maybeGet "Unable to parse parameter calibration unit conversion" (readMaybeText cFactor))
                            (return name)
                        ) =<< parseTokens 2 (T.singleton  ',') (T.pack $ "$P" ++ show i ++ "CALIBRATION") keyvalues)

data FCSMode = List | MultivariateHistogram | UnivariateHistograms deriving (Enum, Show)
toFCSMode :: T.Text -> Get FCSMode
toFCSMode text
    | c == Just (T.singleton 'L') = return List
    | c == Just (T.singleton 'C') = return MultivariateHistogram
    | c == Just (T.singleton 'U') = return UnivariateHistograms
    | otherwise = fail "Unable to parse FCS mode"
    where c = if T.length text == 1 then Just text else Nothing

data ByteOrder = LittleEndian | BigEndian deriving (Enum, Show)
toByteOrder :: T.Text -> Get ByteOrder
toByteOrder text
    | text == T.pack "1,2,3,4" = return LittleEndian
    | text == T.pack "4,3,2,1" = return BigEndian
    | otherwise = fail "Unable to parse byte order"

data Originality = Original | NonDataModified | Appended | DataModified deriving (Enum, Show)
toOriginality :: T.Text -> Get Originality
toOriginality text
    | text == T.pack "Original" = return Original
    | text == T.pack "NonDataModified" = return NonDataModified
    | text == T.pack "Appended" = return Appended
    | text == T.pack "DataModified" = return DataModified
    | otherwise = fail "Unable to parse originality"

data Trigger = Trigger
    { triggerChannel :: T.Text
    , triggerValue :: Int64
    } deriving (Show)

data CellSubset = CellSubset
    { numSimultaneousSubsets :: Int64 -- CSMODE
    , numSubsets :: Maybe Int64 -- CSTOT
    , subsetNBits :: Maybe Int64 -- CSVBITS
    , flagMap :: Map.Map Int64 Int64 -- CSVnFLAG
    } deriving (Show)

maybeCellSubset :: Map.Map T.Text T.Text -> Get (Maybe CellSubset)
maybeCellSubset keyvals = case Map.lookup (T.pack "$CSMODE") keyvals of
        Nothing -> return Nothing
        Just csmode -> do
            numSimultaneousSubsets <- maybeGet "Unable to parse $CSMODE" $ readMaybeText csmode
            numSubsets <- maybeParseKeyval (T.pack "$CSTOT") keyvals
            subsetNBits <- maybeParseKeyval (T.pack "$CSVBITS") keyvals
            let intersectKeyvals = Map.toList $ Map.restrictKeys keyvals $ Set.fromList [T.pack $ "$CSV" ++ show i ++ "FLAG" | i <- [1..numSimultaneousSubsets]]
            flagMap <- Map.fromList <$> mapM (maybeGet2 . \(a,b) -> (readMaybeText $ T.drop 4 $ T.dropEnd 4 a, readMaybeText b)) intersectKeyvals
            return (Just $ CellSubset numSimultaneousSubsets numSubsets subsetNBits flagMap)



data FCSMetadata = FCSMetadata
    { mode :: FCSMode -- MODE
    , byteOrder :: ByteOrder -- BYTEORD
    , nParameters :: Int64 -- PAR
    , parameters :: [Parameter]
    , nEvents :: Int64 -- TOT
    , extraKeyvals :: Map.Map T.Text T.Text
    -- Optional parameters
    , nEventsAborted :: Maybe Int64 -- ABRT
    , acquireTime :: Maybe Time.LocalTime -- BTIM
    , acquireDate :: Maybe Time.Day -- DATE
    , acquireEndTime :: Maybe Time.LocalTime -- ETIM
    , cellSubset :: Maybe CellSubset -- CSMODE, CSTOT, CSVBITS, CSVnFLAG
    , cells :: Maybe T.Text -- CELLS
    , comment :: Maybe T.Text -- COM
    , cytometerType :: Maybe T.Text -- CYT
    , cytometerSerialNumber :: Maybe T.Text -- CYTSN
    , institution :: Maybe T.Text -- INST
    , experimenter :: Maybe T.Text -- EXP
    , operator :: Maybe T.Text -- OP
    , filename :: Maybe T.Text -- FIL
    , lastModified :: Maybe Time.LocalTime -- LAST_MODIFIED
    , lastModifier :: Maybe T.Text -- LAST_MODIFIER
    , nEventsLost :: Maybe Int64 -- LOST
    , originality :: Maybe Originality -- ORIGINALITY
    , plateID :: Maybe T.Text -- PLATEID
    , plateName :: Maybe T.Text -- PLATENAME
    , project :: Maybe T.Text -- PROJ
    , specimen :: Maybe T.Text -- SMNO
--    , spillover :: Maybe T.Text -- SPILLOVER
    , specimenSource :: Maybe T.Text -- SRC
    , computer :: Maybe T.Text -- SYS
    , timestep :: Maybe Float -- TIMESTEP
    , trigger :: Maybe Trigger -- TR
    , volume :: Maybe Float -- VOL
    , wellID :: Maybe T.Text -- WELLID
    } deriving (Show)


getMetadata :: Map.Map T.Text T.Text -> Get FCSMetadata
getMetadata keyvalues = do
    mode <- toFCSMode =<< maybeGet "Missing $MODE parameter" (Map.lookup (T.pack "$MODE") keyvalues)
    byteOrder <- toByteOrder =<< maybeGet "Missing $BYTEORD parameter" (Map.lookup (T.pack "$BYTEORD") keyvalues)
    nParameters <- parseKeyval (T.pack "$PAR") keyvalues
    parameters <- mapM (getParameter keyvalues) [1..nParameters]
    nEvents <- parseKeyval (T.pack "$TOT") keyvalues
    let extraKeyvals = Map.filter ((\case
                                        Just val -> fst val /= '$'
                                        Nothing -> False) . T.uncons) keyvalues
    nEventsAborted <- maybeParseKeyval (T.pack "$ABRT") keyvalues
    acquireStartTime <- maybeGetTime False Time.defaultTimeLocale  "%H:%M:%S%Q" $ Map.lookup (T.pack "$BTIM") keyvalues
    acquireDate <- maybeGetTime False Time.defaultTimeLocale "%d-%b-%0Y" $ Map.lookup (T.pack "$DATE") keyvalues
    acquireEndTime <- maybeGetTime False Time.defaultTimeLocale  "%H:%M:%S%Q" $ Map.lookup (T.pack "$ETIM") keyvalues
    cellSubset <- maybeCellSubset keyvalues
    let cells = Map.lookup (T.pack "$CELLS") keyvalues
    let comment = Map.lookup (T.pack "$COM") keyvalues
    let cytometerType = Map.lookup (T.pack "$CYT") keyvalues
    let cytometerSN = Map.lookup (T.pack "$CYTSN") keyvalues
    let institution = Map.lookup (T.pack "$INST") keyvalues
    let experimenter = Map.lookup (T.pack "$EXP") keyvalues
    let operator = Map.lookup (T.pack "$OP") keyvalues
    let filename = Map.lookup (T.pack "$FIL") keyvalues
    lastModified <- maybeGetTime False Time.defaultTimeLocale  "%d-%b-%0Y %H:%M:%S%Q" $ Map.lookup (T.pack "$LAST_MODIFIED") keyvalues
    let lastModifier = Map.lookup (T.pack "$LAST_MODIFIER") keyvalues
    nEventsLost <- maybeParseKeyval (T.pack "$LOST") keyvalues
    originality <- (\case
                        Nothing -> return Nothing
                        Just val -> Just <$> toOriginality val) $ Map.lookup (T.pack "$ORIGINALITY") keyvalues
    let plateID = Map.lookup (T.pack "$PLATEID") keyvalues
    let plateName = Map.lookup (T.pack "$PLATENAME") keyvalues
    let project = Map.lookup (T.pack "$PROJ") keyvalues
    let specimen = Map.lookup (T.pack "$SMNO") keyvalues

    let specimenSource = Map.lookup (T.pack "$SRC") keyvalues
    let computer = Map.lookup (T.pack "$SYS") keyvalues
    timestep <- maybeParseKeyval (T.pack "$TIMESTEP") keyvalues
    trigger <- (if Map.notMember (T.pack "$TR") keyvalues then return Nothing else (\[tCName, tValue] -> Just <$> fmap (Trigger tCName)
                            (maybeGet "Unable to parse trigger channel value" (readMaybeText tValue))
                        ) =<< parseTokens 2 (T.singleton  ',') (T.pack "$TR") keyvalues)
    volume <- maybeParseKeyval (T.pack "$VOL") keyvalues
    let wellID = Map.lookup (T.pack "$WELLID") keyvalues

    return $! FCSMetadata
                mode byteOrder nParameters parameters nEvents
                extraKeyvals nEventsAborted acquireStartTime
                acquireDate acquireEndTime cellSubset cells comment
                cytometerType cytometerSN institution experimenter
                operator filename lastModified lastModifier nEventsLost
                originality plateID plateName project specimen
                specimenSource computer timestep trigger volume wellID

data FCS = FCS
    { header :: Header
    , primaryText :: TextSegment
    } deriving (Show)

getFCS :: Get FCS
getFCS = do
    initialHeader <- lookAhead getHeader
    textSegment <- lookAhead $
           skip (fromIntegral . start . textOffsets $ initialHeader)
           >> getTextSegment (fromIntegral $ 1 + (end . textOffsets $ initialHeader) - (start . textOffsets $ initialHeader))
    header <- updateHeader initialHeader textSegment
    return $! FCS header textSegment
-- TODO: handle multiple segments by looking at the NEXTDATA offset. We can just consume the input and move on.

readFCS :: FilePath -> IO ()
readFCS filename = do
    input <- BL.readFile filename
    print $ runGet getFCS input

-- Useful trick: (trace . show $ header) return() in do-statement
-- Helper functions
getAsciiString :: Int -> Get [ASCII.Char]
getAsciiString n = getByteString n >>= parseASCII
    where parseASCII bs = case ASCII.byteStringToCharListMaybe bs of
            Nothing -> fail "Unable to decode ASCII string"
            Just val -> return val

readMaybeText :: Read a => T.Text -> Maybe a
readMaybeText = readMaybe . T.unpack

maybeGet :: String -> Maybe a -> Get a
maybeGet errMsg m = case m of
    Just a -> return a
    Nothing -> fail errMsg

maybeGet2 :: (Maybe a, Maybe b) -> Get (a,b)
maybeGet2 text = case text of
    (Just a, Just b) -> return (a,b)
    _ -> fail "Unable to parse tuple"

parseKeyval :: Read a => T.Text -> Map.Map T.Text T.Text -> Get a
parseKeyval key map = case Map.lookup key map of
                Nothing -> fail $ T.unpack key ++ " keyval pair missing!"
                Just val -> maybeGet (T.unpack key ++ " value failed to parse.") (readMaybeText val)

-- Fails if parsing fails, but returns Nothing if given Nothing
maybeGetTime :: Time.ParseTime t => Bool -> Time.TimeLocale -> String -> Maybe T.Text -> Get (Maybe t)
maybeGetTime acceptWhitespace locale format input = case input of
    Just val -> Just <$> maybeGet "Failed to parse datetime" (Time.parseTimeM acceptWhitespace locale format (T.unpack val))
    Nothing -> return Nothing


maybeParseKeyval :: Read a => T.Text -> Map.Map T.Text T.Text -> Get (Maybe a)
maybeParseKeyval key map = case Map.lookup key map of
                    Nothing -> return Nothing
                    Just val -> Just <$> maybeGet (T.unpack key ++ " value failed to parse.") (readMaybeText val)

parseTokens :: Int -> T.Text -> T.Text -> Map.Map T.Text T.Text -> Get [T.Text]
parseTokens n delim key map = case Map.lookup key map of
                Nothing -> fail $ T.unpack key ++ " keyval pair missing!"
                Just allTokens ->
                    if length tokens == n
                    then return tokens
                    else fail $ T.unpack key ++ " value failed to tokenize."
                    where tokens = T.splitOn delim allTokens

maybeParseList :: Read a => T.Text -> T.Text -> Map.Map T.Text T.Text -> Get (Maybe [a])
maybeParseList delim key map = case Map.lookup key map of
                    Nothing -> return Nothing
                    Just val -> return (mapM readMaybeText tokens)
                        where tokens = T.splitOn delim val
