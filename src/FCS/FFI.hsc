{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
module FCS.FFI 
    ( loadFCS
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Matrix as Matrix
import qualified Data.Massiv.Array as A
import Data.Binary.Get (runGet)

import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (malloc)
import Foreign.Marshal.Array (newArray)

import FCS

foreign export ccall loadFCS :: CString -> IO (Ptr FCS)
loadFCS fname = do
    hfname <- peekCString fname
    fcs <- head <$> readFCS hfname
    pFcs <- malloc
    poke pFcs fcs
    return pFcs

-- Export list
#include <fcs.h>
instance Storable T.Text where
    sizeOf _ = #{size StringUTF8}
    alignment _ = #{alignment StringUTF8}
    poke ptr text = do
        let bytes = B.unpack $ Data.Text.Encoding.encodeUtf8 text
        byteArray <- newArray bytes
        #{poke StringUTF8, length} ptr $ length bytes
        #{poke StringUTF8, buffer} ptr $ byteArray
--    peek ptr = do return T.empty

instance Storable (Matrix.Matrix Double) where
    sizeOf _ = #{size DataBuffer}
    alignment _ = #{alignment DataBuffer}
    poke ptr m = do
        byteArray <- newArray $ Matrix.toList m
        #{poke DataBuffer, n_events}     ptr $ Matrix.nrows m
        #{poke DataBuffer, n_parameters} ptr $ Matrix.ncols m
        #{poke DataBuffer, data}         ptr $ byteArray
--  peek ptr

instance Storable (A.Matrix A.S Double) where
    sizeOf _ = #{size DataBuffer}
    alignment _ = #{alignment DataBuffer}
    poke ptr m = do
        #{poke DataBuffer, n_events}     ptr $ (A.unSz . fst $ A.unconsSz $ A.size m)
        #{poke DataBuffer, n_parameters} ptr $ (A.unSz . snd $ A.unconsSz $ A.size m)
--  peek ptr

instance Storable FCS where
    sizeOf _ = #{size FCSFile}
    alignment _ = #{alignment FCSFile}
    poke ptr f = do
        #{poke FCSFile, name}          ptr $ T.pack "Test"
        #{poke FCSFile, uncompensated} ptr $ uncompensated $ dataSegment f
        #{poke FCSFile, compensated}   ptr $ compensated $ dataSegment f