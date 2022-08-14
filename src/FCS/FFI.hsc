{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
module FCS.FFI 
    ( loadFCS,
      freeFCS
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Matrix as Matrix
import qualified Data.Massiv.Array as A
import Data.Binary.Get (runGet)
import Data.Vector.Storable (unsafeWith)

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

foreign export ccall freeFCS :: Ptr FCS -> IO Int
freeFCS pFcs = return 0

-- Export list
#include <fcs.h>


-----------Helper functions-----------------------------------
fcsModeToEnum :: FCSMode -> Int
fcsModeToEnum m
    | m == List                     = #{const mode_List}
    | m == MultivariateHistogram    = #{const mode_MultivariateHistogram}
    | m == UnivariateHistograms     = #{const mode_UnivariateHistograms}

datatypeToEnum :: Datatype -> Int
datatypeToEnum d
    | d == StoredInteger = #{const type_StoredInteger}
    | d == StoredFloat   = #{const type_StoredFloat}
    | d == StoredDouble  = #{const type_StoredDouble}
    | d == StoredASCII   = #{const type_StoredASCII}

byteOrderToEnum :: ByteOrder -> Int
byteOrderToEnum bo
    | bo == LittleEndian    = #{const LittleEndian}
    | bo == BigEndian       = #{const BigEndian}

-----------STRUCT DEFINITIONS-----------------------------------------
instance Storable T.Text where
    sizeOf _ = #{size StringUTF8}
    alignment _ = #{alignment StringUTF8}
    poke ptr text = do
        let bytes = B.unpack $ Data.Text.Encoding.encodeUtf8 text
        byteArray <- newArray bytes
        #{poke StringUTF8, length} ptr $ length bytes
        #{poke StringUTF8, buffer} ptr $ byteArray
--    peek ptr = do return T.empty

instance Storable (Maybe T.Text) where
    sizeOf _ = #{size OptionalString}
    alignment _ = #{alignment OptionalString}
    poke ptr t = case t of
        Nothing -> do
            #{poke OptionalString, valid}  ptr $ False
        Just text -> do
            #{poke OptionalString, valid}  ptr $ True
            #{poke OptionalString, string} ptr $ text
--  peek ptr

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
        -- #{poke ...} has type signature
        -- Ptr a -> Storable b -> IO()
        unsafeWith (A.toStorableVector m) (\mptr -> #{poke DataBuffer, data} ptr mptr)
--  peek ptr

instance Storable Parameter where
    sizeOf _ = #{size Parameter}
    alignment _ = #{alignment Parameter}
    poke ptr p = do
        #{poke Parameter, bit_length}   ptr $ bitLength p
        #{poke Parameter, short_name}   ptr $ shortName p

instance Storable FCSMetadata where
    sizeOf _ = #{size FCSMetadata}
    alignment _ = #{alignment FCSMetadata}
    poke ptr m = do
        paramArray <- newArray $ parameters m
        #{poke FCSMetadata, mode} ptr $ fcsModeToEnum $ mode m
        #{poke FCSMetadata, datatype} ptr $ datatypeToEnum $ datatype m
        #{poke FCSMetadata, byte_order} ptr $ byteOrderToEnum $ byteOrder m
        #{poke FCSMetadata, n_parameters} ptr $ nParameters m
        #{poke FCSMetadata, parameters} ptr $ paramArray
--  peek ptr

instance Storable FCS where
    sizeOf _ = #{size FCSFile}
    alignment _ = #{alignment FCSFile}
    poke ptr f = do
        #{poke FCSFile, name}          ptr $ T.pack "Test"
        #{poke FCSFile, metadata}      ptr $ metadata f
        #{poke FCSFile, uncompensated} ptr $ uncompensated $ dataSegment f
        #{poke FCSFile, compensated}   ptr $ compensated $ dataSegment f