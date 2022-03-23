module FCS.Shim
    ( shimKeyvals
    , shimParameterScale
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

shimKeyvals :: Map.Map T.Text T.Text -> Map.Map T.Text T.Text
shimKeyvals = Map.filterWithKey (\k v -> not $
    T.isPrefixOf (T.pack "$P") k &&
    T.isSuffixOf (T.pack "V") k &&
    v == T.pack "NA")

shimParameterScale :: Float -> Float -> Float
shimParameterScale x y
    | x == 0 && y /= 0 = 1.0
    | otherwise = y