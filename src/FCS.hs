{-# LANGUAGE LambdaCase #-}
module FCS
    ( someFunc
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Int ( Int32, Int64 )
import Data.Binary.Get (Get, getByteString, skip, lookAhead, Decoder(Fail))
import qualified ASCII
import qualified ASCII.Char as AC
import Text.Read (readMaybe)
import Data.Void (Void)
import Control.Monad (liftM)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding

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
    , dataOffsets :: SegmentOffsets
    , analysisOffsets :: SegmentOffsets
    } deriving (Show)

getHeader :: Get Header
getHeader = Header
    <$> getVersion
    <* (getAsciiString 4 >>= (\cs -> if all (== AC.Space) cs then return True else fail "Invalid character in header spacer"))
    <*> getSegmentOffsets
    <*> getSegmentOffsets
    <*> getSegmentOffsets

data TextSegment = TextSegment
    { delimiter :: AC.Char
    , keyvals   :: Map.Map T.Text T.Text
    }

getSegmentDelimiter :: T.Text -> Maybe Char
getSegmentDelimiter text = case (mfirst, mlast) of
                            (Just first, Just last) -> if fst first == snd last then Just $ fst first else Nothing
                            (_,_) -> Nothing
                           where mfirst = T.uncons text
                                 mlast = T.unsnoc text

_splitSegmentByEscapedDelims :: Char -> T.Text -> T.Text -> [T.Text]
_splitSegmentByEscapedDelims delim token text = pretoken : _splitSegmentByEscapedDelims delim T.empty next
                    where tdelim = T.singleton delim
                          (pretoken, next) = T.breakOn tdelim text

splitSegmentByEscapedDelims :: Char -> T.Text -> [[T.Text]]
splitSegmentByEscapedDelims delim text = map (T.splitOn tdelim) splits
                    where tdelim = T.singleton delim
                          splits = T.splitOn (T.snoc tdelim delim) text

zipEveryOther :: [T.Text] -> [(T.Text, T.Text)]
zipEveryOther (x:y:rest) = (x,y) : zipEveryOther rest
zipEveryOther [x] = error "Cannot every-other zip a odd-numbered list"
zipEveryOther [] = []

--getSegmentKeyvals :: Char -> T.Text -> Maybe [(T.Text, T.Text)]
--getSegmentKeyvals delim text = if (len < 3) || odd len then Nothing else Just $ zipEveryOther $ normalizeSegmentDelims delim $ tail $ init splits
--                               where splits = T.splitOn (T.singleton delim) text
--                                     len = length splits

getTextSegment :: Int -> Get TextSegment
getTextSegment length = do
    segment <- Data.Text.Encoding.decodeUtf8 <$> getByteString length
    delimiter <- maybeGet "Text segment did not start and end with the same delimiter!" $ getSegmentDelimiter segment
    -- Remember to validate that the keyword keys are plain ASCII
    return $! TextSegment AC.Comma Map.empty
-- Replace with just a lot of pattern matching on text.
-- Have a pattern that matches the exterior delimiters, delegating to a pattern that matches a properly delimited string like delim:notdelim:rest:notdelim:delim


someFunc :: IO ()
someFunc = putStrLn "someFunc"

getAsciiString :: Int -> Get [ASCII.Char]
getAsciiString n = getByteString n >>= parseASCII
    where parseASCII bs = case ASCII.byteStringToCharListMaybe bs of
            Nothing -> fail "Unable to decode ASCII string"
            Just val -> return val

lookaheadReadFromOffset :: Int -> Get a -> Get a
lookaheadReadFromOffset n decoder = skip n *> lookAhead decoder

maybeGet :: String -> Maybe a -> Get a
maybeGet errMsg m = case m of
    Just a -> return a
    Nothing -> fail errMsg
