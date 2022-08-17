{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module FCS
    ( readFCS
    , FCS
    , FCSMode (..)
    , Datatype (..)
    , ByteOrder (..)
    , VisualizationScale (..)
    , Originality (..)
    , ParameterVisualizationScale
    , FCSMetadata
    , Parameter
    , AmplificationType
    , ParameterCalibration
    , Spillover
    , Trigger
    , compensated
    , uncompensated
    , dataSegment
    , metadata
    , mode
    , datatype
    , byteOrder
    , nParameters
    , parameters
    , bitLength
    , shortName
    , range
    , amplification
    , logDecades
    , offset
    , vizScale
    , scale
    , f1
    , f2
    , FCS.filter
    , gain
    , excitationWavelength
    , excitationPower
    , percentLightCollected
    , name
    , detectorType
    , detectorVoltage
    , unitConversionFactor
    , unitName
    , calibration
    , nEventsAborted
    , acquireTime
    , acquireEndTime
    , acquireDate
    , cells
    , comment
    , cytometerType
    , cytometerSerialNumber
    , institution
    , experimenter
    , operator
    , filename
    , lastModified
    , lastModifier
    , nEventsLost
    , originality
    , plateID
    , plateName
    , project
    , specimen
    , spillover
    , specimenSource
    , computer
    , timestep
    , trigger
    , wellID
    , nParams
    , parameterNames
    , spilloverMatrix
    , triggerChannel
    , triggerValue
    ) where

import Control.Monad (replicateM, liftM2, liftM3)
import Data.Time as Time
import qualified Data.ByteString.Lazy as BL
import Data.List (elemIndex)
import Data.Int ( Int32, Int64 )
import Data.Binary.Get (Get, getByteString, getFloatle, getFloatbe, getDoublele, getDoublebe, skip, lookAhead, runGet, isolate)
import qualified ASCII
import qualified ASCII.Char as AC
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Matrix as DM
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Core as MC
import Data.Massiv.Core.Index (Dimension (Dim1, Dim2), getDimension, unSz, Ix2 ((:.)))
import Data.Char (isAscii)
import Debug.Trace ( trace, traceM )

import qualified FCS.Shim as Shim
import GHC.Float (float2Double)
import qualified FCS.Shim as FCS
import Data.Maybe (isJust, fromJust)
import Data.Massiv.Array ((<!?), (.><.))

type SMatrixFloat = A.Matrix A.S Float
type SMatrixDouble = A.Matrix A.S Double

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
zipEveryOther [_] = error "Cannot every-other zip a odd-numbered list"
zipEveryOther [] = []

getSegmentKeyvals :: AC.Char -> T.Text -> Maybe [(T.Text, T.Text)]
getSegmentKeyvals delim text = if odd len then Nothing else Just $ zipEveryOther splits
                               where splits = splitSegmentByEscapedDelims (toEnum $ AC.toInt delim) text
                                     len = length splits

textToDelimiter :: T.Text -> Maybe Char
textToDelimiter text = case mfirst of
                            Just first -> Just $ fst first
                            Nothing -> Nothing
                           where mfirst = T.uncons text

getSegmentDelimiter :: Get AC.Char
getSegmentDelimiter = do
    firstChar <- Data.Text.Encoding.decodeUtf8 <$> getByteString 1
    delimiter <- maybeGet "Text segment delimiter is invalid!" $ textToDelimiter firstChar
    maybeGet "Delimiter is not an ASCII character" $ AC.fromIntMaybe . fromEnum $ delimiter

getTextSegment :: Int -> AC.Char -> Get TextSegment
getTextSegment length delimiter = do
    segment <- Data.Text.Encoding.decodeUtf8 <$> getByteString length
    keyvals <- maybeGet "Text segment has invalid key-value specification" $ getSegmentKeyvals delimiter segment
    if all (T.all isAscii . fst) keyvals then return () else fail "Specified keys are not ASCII"
    return $! TextSegment delimiter $ Map.fromList keyvals


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

data VisualizationScale = Linear | Logarithmic deriving (Enum, Show, Eq)
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
getParameter keyvalues i = do
                bitLength <- parseKeyval (T.pack $ "$P" ++ show i ++ "B") keyvalues
                amplification <- (\[decades, offset] -> liftM2 AmplificationType
                        (maybeGet "Unable to parse amplification decades type" (readMaybeText decades))
                        (maybeGet "Unable to parse amplification offset" (readMaybeText offset))
                    ) =<< parseTokens 2 (T.singleton ',') (T.pack $ "$P" ++ show i ++ "E") keyvalues
                shortName <- maybeGet ("$P" ++ show i ++ "N key missing!") (Map.lookup (T.pack $ "$P" ++ show i ++ "N") keyvalues)
                range <- parseKeyval (T.pack $ "$P" ++ show i ++ "R") keyvalues
                unshimmedVizScale <- (if Map.notMember (T.pack $ "$P" ++ show i ++ "D") keyvalues then return Nothing else (\[scale, f1, f2] -> Just <$> liftM3 ParameterVisualizationScale
                                (toVizScale scale)
                                (maybeGet "Unable to parse visualization parameter 1" (readMaybeText f1))
                                (maybeGet "Unable to parse visualization parameter 2" (readMaybeText f2))
                    ) =<< parseTokens 3 (T.singleton ',') (T.pack $ "$P" ++ show i ++ "D") keyvalues)
                let vizScale = (\case
                                    Just vs -> Just $ ParameterVisualizationScale (scale vs) (f1 vs) (FCS.shimParameterScale (f1 vs) (f2 vs))
                                    Nothing -> Nothing) unshimmedVizScale
                let filter = Map.lookup (T.pack $ "$P" ++ show i ++ "F") keyvalues
                gain <- maybeParseKeyval (T.pack $ "$P" ++ show i ++ "G") keyvalues
                excitationWavelength <-  maybeParseList (T.singleton ',') (T.pack $ "$P" ++ show i ++ "L") keyvalues
                excitationPower <- maybeParseKeyval (T.pack $ "$P" ++ show i ++ "O") keyvalues
                percentLightCollected <- maybeParseKeyval (T.pack $ "$P" ++ show i ++ "P") keyvalues
                let name = Map.lookup (T.pack $ "$P" ++ show i ++ "S") keyvalues
                let detectorType = Map.lookup (T.pack $ "$P" ++ show i ++ "T") keyvalues
                detectorVoltage <- maybeParseKeyval (T.pack $ "$P" ++ show i ++ "V") keyvalues
                calibration <- (if Map.notMember (T.pack $ "$P" ++ show i ++ "CALIBRATION") keyvalues then return Nothing else (\[cFactor, name] -> Just <$> liftM2 ParameterCalibration
                                    (maybeGet "Unable to parse parameter calibration unit conversion" (readMaybeText cFactor))
                                    (return name)
                                ) =<< parseTokens 2 (T.singleton  ',') (T.pack $ "$P" ++ show i ++ "CALIBRATION") keyvalues)
                return $! Parameter bitLength amplification shortName range
                                    vizScale filter gain excitationWavelength
                                    excitationPower percentLightCollected name
                                    detectorType detectorVoltage calibration

data FCSMode = List | MultivariateHistogram | UnivariateHistograms deriving (Enum, Show, Eq)
toFCSMode :: T.Text -> Get FCSMode
toFCSMode text
    | c == Just (T.singleton 'L') = return List
    | c == Just (T.singleton 'C') = return MultivariateHistogram
    | c == Just (T.singleton 'U') = return UnivariateHistograms
    | otherwise = fail "Unable to parse FCS mode"
    where c = if T.length text == 1 then Just text else Nothing

data Datatype = StoredInteger | StoredFloat | StoredDouble | StoredASCII deriving (Enum, Show, Eq)
toDatatype :: T.Text -> Get Datatype
toDatatype text
    | c == Just (T.singleton 'I') = return StoredInteger
    | c == Just (T.singleton 'F') = return StoredFloat
    | c == Just (T.singleton 'D') = return StoredDouble
    | c == Just (T.singleton 'A') = return StoredASCII
    | otherwise = fail "Unable to parse datatype"
    where c = if T.length text == 1 then Just text else Nothing

data ByteOrder = LittleEndian | BigEndian deriving (Enum, Show, Eq)
toByteOrder :: T.Text -> Get ByteOrder
toByteOrder text
    | text == T.pack "1,2,3,4" = return LittleEndian
    | text == T.pack "4,3,2,1" = return BigEndian
    | otherwise = fail "Unable to parse byte order"

data Originality = Original | NonDataModified | Appended | DataModified deriving (Enum, Show, Eq)
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


data Spillover = Spillover
    { nParams :: Int64
    , parameterNames :: [T.Text]
    , spilloverMatrix :: SMatrixDouble
    } deriving (Show)

getSpillover :: T.Text -> Get Spillover
getSpillover val = do
    n <- maybeGet "Unable to parse number of spillover parameters" $ readMaybeText $ head tokens
    if length tokens /= 1 + n + n * n then fail "Invalid number of spillover parameters" else do
        let parameterNames = (take n . drop 1) tokens
        spilloverLinear <- A.fromList A.Seq
                        <$> maybeGet "Unable to parse spillover matrix"
                            (mapM readMaybeText (take (n * n) . drop (1 + n) $ tokens))
        spillover <- A.resizeM (A.Sz (n :. n)) spilloverLinear
        return $! Spillover (fromIntegral n) parameterNames spillover
    where tokens = T.splitOn (T.singleton ',') val


data FCSMetadata = FCSMetadata
    { mode :: FCSMode -- MODE
    , datatype :: Datatype -- DATATYPE
    , byteOrder :: ByteOrder -- BYTEORD
    , nParameters :: Int64 -- PAR
    , parameters :: [Parameter]
    , nEvents :: Int64 -- TOT
    , extraKeyvals :: Map.Map T.Text T.Text
    -- Optional parameters
    , nEventsAborted :: Maybe Int64 -- ABRT
    , acquireTime :: Maybe Time.TimeOfDay -- BTIM
    , acquireDate :: Maybe Time.Day -- DATE
    , acquireEndTime :: Maybe Time.TimeOfDay -- ETIM
    , cellSubset :: Maybe CellSubset -- CSMODE, CSTOT, CSVBITS, CSVnFLAG
    , cells :: Maybe T.Text -- CELLS
    , comment :: Maybe T.Text -- COM
    , cytometerType :: Maybe T.Text -- CYT
    , cytometerSerialNumber :: Maybe T.Text -- CYTSN
    , institution :: Maybe T.Text -- INST
    , experimenter :: Maybe T.Text -- EXP
    , operator :: Maybe T.Text -- OP
    , filename :: Maybe T.Text -- FIL
-- GATE info
-- PKn
-- PKNn
-- RnI
-- RnW
    , lastModified :: Maybe Time.TimeOfDay -- LAST_MODIFIED
    , lastModifier :: Maybe T.Text -- LAST_MODIFIER
    , nEventsLost :: Maybe Int64 -- LOST
    , originality :: Maybe Originality -- ORIGINALITY
    , plateID :: Maybe T.Text -- PLATEID
    , plateName :: Maybe T.Text -- PLATENAME
    , project :: Maybe T.Text -- PROJ
    , specimen :: Maybe T.Text -- SMNO
    , spillover :: Maybe Spillover -- SPILLOVER
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
    datatype <- toDatatype =<< maybeGet "Missing $DATATYPE parameter" (Map.lookup (T.pack "$DATATYPE") keyvalues)
    byteOrder <- toByteOrder =<< maybeGet "Missing $BYTEORD parameter" (Map.lookup (T.pack "$BYTEORD") keyvalues)
    nParameters <- parseKeyval (T.pack "$PAR") keyvalues
    parameters <- mapM (getParameter keyvalues) [1..nParameters]
    nEvents <- parseKeyval (T.pack "$TOT") keyvalues
    let extraKeyvals = Map.filterWithKey (\k _ -> (\case
                                        Just val -> fst val /= '$'
                                        Nothing -> False) . T.uncons $ k) keyvalues
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
    spillover <- (\case
                    Nothing -> return Nothing
                    Just t -> Just <$> getSpillover t) $ Map.lookup (T.pack "$SPILLOVER") keyvalues
    let specimenSource = Map.lookup (T.pack "$SRC") keyvalues
    let computer = Map.lookup (T.pack "$SYS") keyvalues
    timestep <- maybeParseKeyval (T.pack "$TIMESTEP") keyvalues
    trigger <- (if Map.notMember (T.pack "$TR") keyvalues then return Nothing else (\[tCName, tValue] -> Just <$> fmap (Trigger tCName)
                            (maybeGet "Unable to parse trigger channel value" (readMaybeText tValue))
                        ) =<< parseTokens 2 (T.singleton  ',') (T.pack "$TR") keyvalues)
    volume <- maybeParseKeyval (T.pack "$VOL") keyvalues
    let wellID = Map.lookup (T.pack "$WELLID") keyvalues

    return $! FCSMetadata
                mode datatype byteOrder nParameters parameters nEvents
                extraKeyvals nEventsAborted acquireStartTime
                acquireDate acquireEndTime cellSubset cells comment
                cytometerType cytometerSN institution experimenter
                operator filename lastModified lastModifier nEventsLost
                originality plateID plateName project specimen spillover
                specimenSource computer timestep trigger volume wellID

data DataSegment = DataSegment
    { unprocessed :: SMatrixDouble
    , uncompensated :: SMatrixDouble
    , compensated :: SMatrixDouble
    }
instance Show DataSegment where
    show x = "{num_parameters: " ++ show (getDimension (unSz $ A.size $ uncompensated x) Dim1)
           ++", num_events: " ++ show (getDimension (unSz $ A.size $ uncompensated x) Dim2) ++ "}"

getRawData :: FCSMetadata -> Get [Double]
getRawData meta
    | dtype == StoredDouble && endianness == LittleEndian = replicateM len getDoublele
    | dtype == StoredDouble && endianness == BigEndian    = replicateM len getDoublebe
    | dtype == StoredFloat  && endianness == LittleEndian = replicateM len (float2Double <$> getFloatle)
    | dtype == StoredFloat  && endianness == BigEndian    = replicateM len (float2Double <$> getFloatbe)
    | otherwise = fail "Unsupported datatype"
    where dtype = datatype meta
          endianness = byteOrder meta
          len = fromIntegral $ nParameters meta * nEvents meta

gainLogTransform :: Parameter -> Double -> Double
gainLogTransform param
    | isJust pGain = \x -> x / fromJust pGain
    | pLogDecades /= 0.0 = \x -> 10**(pLogDecades * x / pRange) * pOffset
    | otherwise = id
    where pGain = float2Double <$> gain param :: Maybe Double
          pLogDecades = float2Double $ logDecades $ amplification param :: Double
          pOffset = float2Double $ offset $ amplification param :: Double
          pRange = fromIntegral $ range param :: Double

getCalibrationTransform :: Parameter -> Double -> Double
getCalibrationTransform param = \case
                                    Just cal -> \x -> x * float2Double (unitConversionFactor cal)
                                    Nothing -> id
                                $ calibration param


applyCompensationSlice :: (Int, A.Array A.D A.Ix1 Double) -> A.Array A.DL A.Ix2 Double -> Get (A.Array A.DL A.Ix2 Double)
applyCompensationSlice slice x = uncurry (A.replaceSlice 1) slice (A.computeAs A.B x)

compensateData :: [Parameter] -> Spillover -> A.Matrix A.S Double -> Get SMatrixDouble
compensateData params spill uncomp = do
    -- Extract the indicies of the parameters needing compensation
    paramIndicies <- maybeGet "Invalid parameter name in spillover matrix" $ mapM (`elemIndex` map shortName params) (parameterNames spill)
    -- Invert the spillover matrix to get a compensation matrix. Make it manifest
    compMatrix <- A.computeAs A.B <$> massivInvert (spilloverMatrix spill)
    -- Extract column vectors from uncompensated data
    precompSlices <- mapM (uncomp <!?) paramIndicies
    -- Re-stack column vectors into a matrix and make it manifest for matrix multiplication
    precomp <- A.computeAs A.B <$>  A.stackInnerSlicesM precompSlices
    -- Separate the compensated data into columns, pairing with the original parameter index
    compSlices <- A.zip (A.fromList A.Seq paramIndicies :: A.Array A.B A.Ix1 Int) . A.innerSlices <$> (precomp .><. compMatrix)
    A.computeAs A.S <$> A.foldrM applyCompensationSlice (A.toLoadArray uncomp) compSlices

getData :: FCSMetadata -> Get DataSegment
getData meta = do
    unprocessedList <- getRawData meta
    unprocessed <- A.resizeM (A.Sz (nE :. nP)) . A.fromList A.Seq $ unprocessedList
    --traceM ("Size of unprocessed: " ++ show (A.size unprocessed))
    let cols = A.innerSlices unprocessed
    let mapped = A.imap (\i col -> let param = parameters meta !! i
                                   in A.map (getCalibrationTransform param . gainLogTransform param) col) cols
    processed <- A.computeAs A.S <$> A.stackInnerSlicesM mapped
    --traceM ("Size of processed: " ++ show (A.size processed))
    compensated <- \case
                      Just spill -> compensateData (parameters meta) spill processed
                      Nothing -> return processed
                    $ spillover meta
    --traceM ("Size of compensated: " ++ show (A.size compensated))
    return $! DataSegment unprocessed processed compensated
    where nP = fromIntegral $ nParameters meta
          nE = fromIntegral $ nEvents meta

data FCS = FCS
    { header :: Header
    , primaryText :: TextSegment
    , supplementalText :: Maybe TextSegment
    , dataSegment :: DataSegment
    , analysisSegment :: Maybe TextSegment
    , metadata :: FCSMetadata
    } deriving (Show)

getSingleFCS :: Get (FCS, Int)
getSingleFCS = do
    initialHeader <- lookAhead getHeader
    delimiter <- lookAhead $ skip (fromIntegral . start . textOffsets $ initialHeader) >> getSegmentDelimiter
    textSegment <- lookAhead $
           skip (fromIntegral . start . textOffsets $ initialHeader)
           >> getTextSegment (fromIntegral $ 1 + (end . textOffsets $ initialHeader) - (start . textOffsets $ initialHeader)) delimiter
    header <- updateHeader initialHeader textSegment
    supplementalText <- if (start . stextOffsets $ header) == 0 && (end . stextOffsets $ header) == 0 then return Nothing else
                lookAhead $ skip (fromIntegral . start . stextOffsets $ header)
                >> Just <$> getTextSegment (fromIntegral $ 1 + (end . stextOffsets $ header) - (start . stextOffsets $ header)) delimiter
    analysisSegment <- if (start . analysisOffsets $ header) == 0 && (end . analysisOffsets $ header) == 0 then return Nothing else
                lookAhead $ skip (fromIntegral . start . analysisOffsets $ header)
                >> Just <$> getTextSegment (fromIntegral $ 1 + (end . analysisOffsets $ header) - (start . analysisOffsets $ header)) delimiter
    nextOffset <- parseKeyval (T.pack "$NEXTDATA") $ keyvals textSegment
    let preshimMetadataMap = \case
                        Just stext -> Map.union (keyvals textSegment) (keyvals stext)
                        Nothing -> keyvals textSegment
                      $ supplementalText
    let metadataMap = Shim.shimKeyvals preshimMetadataMap
    metadata <- getMetadata metadataMap
    dataSegment <- lookAhead $
        skip (fromIntegral . start . dataOffsets $ header)
        >> isolate (fromIntegral $ 1 + (end . dataOffsets $ header) - (start . dataOffsets $ header)) (getData metadata)
    return (FCS header textSegment supplementalText dataSegment analysisSegment metadata, nextOffset)

getFCS :: Get [FCS]
getFCS = do
    (fcs, next) <- getSingleFCS
    rest <- if next /= 0 then skip next >> getFCS else return []
    return (fcs : rest)


readFCS :: FilePath -> IO [FCS]
readFCS filename = do
    input <- BL.readFile filename
    return (runGet getFCS input)

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

eitherGet :: String -> Either String b -> Get b
eitherGet errMsg m = case m of
    Left a -> fail $ errMsg ++ ":" ++ a
    Right b -> return b

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

mapEveryCol :: (Int -> a -> a) -> DM.Matrix a -> DM.Matrix a
-- Maps the same function over an entire column, passing it the _column_ index
mapEveryCol mapF initial = foldl (flip ($)) initial [DM.mapCol (\_ x -> mapF i x) i | i <- [1..DM.ncols initial]]


massivInvert :: A.Matrix A.S Double -> Get(A.Matrix A.P Double)
massivInvert x = do
    let sz = A.size x
    let ix = unSz sz
    let dmX = DM.fromList (getDimension ix Dim1) (getDimension ix Dim2) $ A.toList x
    xinv <- eitherGet "Uninvertible spillover matrix" $ DM.inverse dmX
    A.resizeM sz $ A.fromList A.Seq (DM.toList xinv)

instance A.MonadThrow Get where
    throwM e = fail (show e)