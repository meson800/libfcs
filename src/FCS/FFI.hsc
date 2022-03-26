{-# LANGUAGE ForeignFunctionInterface #-}
module FCS.FFI where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding

import Foreign.Storable
import Foreign.Marshal.Array (newArray)

-- Export list
#include <fcs.h>
instance Storable T.Text where
    sizeOf _ = #{size StringUTF8}
    alignment _ = #{alignment StringUTF8}
    poke ptr text = do
        let bytes = B.unpack $ Data.Text.Encoding.encodeUtf8 text
        byteArray <- newArray bytes
        #{poke StringUTF8, buffer} ptr $ byteArray
        #{poke StringUTF8, length} ptr $ length bytes
    peek ptr = do return T.empty