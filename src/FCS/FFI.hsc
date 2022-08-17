{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
module FCS.FFI 
    ( loadFCS,
      freeFCS
    ) where

import Data.Int ( Int32, Int64 )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding
import qualified Data.Map.Strict as Map
import qualified Data.Matrix as Matrix
import qualified Data.Massiv.Array as A
import Data.Binary.Get (runGet)
import Data.Vector.Storable (unsafeWith)
import Data.Word (Word8)
import Data.Time.Format.ISO8601 (iso8601Show)

import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray, mallocArray, copyArray)

import Debug.Trace ( trace, traceM )

import FCS

#include <fcs.h>

foreign export ccall loadFCS :: CString -> IO (Ptr FCS)
loadFCS fname = do
    hfname <- peekCString fname
    fcs <- head <$> readFCS hfname
    pFcs <- malloc
    poke pFcs fcs
    return pFcs


foreign export ccall freeFCS :: Ptr FCS -> IO Int
freeFCS pFcs = do
    -- Example of how to free all of the various pointers stuffed inside here
    -- Free key float databuffers
    uncompPtr <- #{peek FCSFile, uncompensated.data} pFcs :: IO (Ptr Word8)
    free uncompPtr
    compPtr <- #{peek FCSFile, compensated.data} pFcs :: IO (Ptr Word8)
    free compPtr
    -- TODO: free other optionals
    -- TODO: free parameters
    -- Free overall datastructure
    free pFcs
    return 0


-----------Helper functions-----------------------------------
fcsModeToEnum :: FCSMode -> Int64
fcsModeToEnum m
    | m == List                     = #{const mode_List}
    | m == MultivariateHistogram    = #{const mode_MultivariateHistogram}
    | m == UnivariateHistograms     = #{const mode_UnivariateHistograms}

datatypeToEnum :: Datatype -> Int64
datatypeToEnum d
    | d == StoredInteger = #{const type_StoredInteger}
    | d == StoredFloat   = #{const type_StoredFloat}
    | d == StoredDouble  = #{const type_StoredDouble}
    | d == StoredASCII   = #{const type_StoredASCII}

byteOrderToEnum :: ByteOrder -> Int64
byteOrderToEnum bo
    | bo == LittleEndian    = #{const LittleEndian}
    | bo == BigEndian       = #{const BigEndian}

vizScaleToEnum :: VisualizationScale -> Int64
vizScaleToEnum v
    | v == Linear       = #{const viz_Linear}
    | v == Logarithmic  = #{const viz_Logarithmic}

originalityToEnum :: Originality -> Int64
originalityToEnum o
    | o == Original         = #{const orig_Original}
    | o == NonDataModified  = #{const orig_NonDataModified}
    | o == Appended         = #{const orig_Appended}
    | o == DataModified     = #{const orig_DataModified}

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
            #{poke OptionalString, present} ptr $ False
        Just text -> do
            #{poke OptionalString, present} ptr $ True
            #{poke OptionalString, string}  ptr $ text
--  peek ptr

instance Storable (Maybe Float) where
    sizeOf _ = #{size OptionalFloat}
    alignment _ = #{alignment OptionalFloat}
    poke ptr f= case f of
        Nothing -> do
            #{poke OptionalFloat, present} ptr $ False
        Just f' -> do
            #{poke OptionalFloat, present} ptr $ True
            #{poke OptionalFloat, value}   ptr $ f'
--  peek ptr

instance Storable (Maybe Int64) where
    sizeOf _ = #{size OptionalInt64}
    alignment _ = #{alignment OptionalInt64}
    poke ptr i = case i of
        Nothing -> do
            #{poke OptionalInt64, present} ptr $ False
        Just i' -> do
            #{poke OptionalInt64, present} ptr $ True
            #{poke OptionalInt64, value}   ptr $ i'

instance Storable (Matrix.Matrix Double) where
    sizeOf _ = #{size DataBuffer}
    alignment _ = #{alignment DataBuffer}
    poke ptr m = do
        byteArray <- newArray $ Matrix.toList m
        #{poke DataBuffer, n_rows}     ptr $ Matrix.nrows m
        #{poke DataBuffer, n_cols} ptr $ Matrix.ncols m
        #{poke DataBuffer, data}         ptr $ byteArray
--  peek ptr

instance Storable (A.Matrix A.S Double) where
    sizeOf _ = #{size DataBuffer}
    alignment _ = #{alignment DataBuffer}
    poke ptr m = do
        #{poke DataBuffer, n_rows}  ptr $ (A.unSz . fst $ A.unconsSz $ A.size m)
        #{poke DataBuffer, n_cols}  ptr $ (A.unSz . snd $ A.unconsSz $ A.size m)
        -- #{poke ...} has type signature
        -- Ptr a -> Storable b -> IO()
        let n_elem = A.elemsCount m
        byteArray <- mallocArray n_elem :: IO (Ptr Double)
        unsafeWith (A.toStorableVector m) $ (\ptr -> copyArray byteArray ptr n_elem)
        #{poke DataBuffer, data} ptr $ byteArray
--  peek ptr

instance Storable AmplificationType where
    sizeOf _ = #{size AmplificationType}
    alignment _ = #{alignment AmplificationType}
    poke ptr at = do
        #{poke AmplificationType, log_decades}   ptr $ logDecades at
        #{poke AmplificationType, offset}        ptr $ offset at

instance Storable (Maybe ParameterVisualizationScale) where
    sizeOf _ = #{size OptionalVizScale}
    alignment _ = #{alignment OptionalVizScale}
    poke ptr s = case s of
        Nothing -> do
            #{poke OptionalVizScale, present}   ptr $ False
        Just s' -> do
            #{poke OptionalVizScale, present}   ptr $ True
            #{poke OptionalVizScale, viz_scale} ptr $ vizScaleToEnum $ scale s'
            #{poke OptionalVizScale, f1}        ptr $ f1 s'
            #{poke OptionalVizScale, f2}        ptr $ f2 s'

instance Storable (Maybe [Int64]) where
    sizeOf _ = #{size OptionalInt64Array}
    alignment _ = #{alignment OptionalInt64Array}
    poke ptr x = case x of
        Nothing -> do
            #{poke OptionalInt64Array, present} ptr $ False
        Just ints -> do
            intArray <- newArray $ ints
            #{poke OptionalInt64Array, present} ptr $ True
            #{poke OptionalInt64Array, vals}    ptr $ intArray

instance Storable (Maybe ParameterCalibration) where
    sizeOf _ = #{size OptionalParamCalibration}
    alignment _ = #{alignment OptionalParamCalibration}
    poke ptr c = case c of
        Nothing -> do
            #{poke OptionalParamCalibration, present}   ptr $ False
        Just c' -> do
            #{poke OptionalParamCalibration, present}   ptr $ True
            #{poke OptionalParamCalibration, unit_conversion_factor} ptr $ unitConversionFactor c'
            #{poke OptionalParamCalibration, unit_name}        ptr $ unitName c'

instance Storable Parameter where
    sizeOf _ = #{size Parameter}
    alignment _ = #{alignment Parameter}
    poke ptr p = do
        #{poke Parameter, bit_length}               ptr $ bitLength p
        #{poke Parameter, amplification}            ptr $ amplification p
        #{poke Parameter, short_name}               ptr $ shortName p
        #{poke Parameter, range}                    ptr $ range p
        #{poke Parameter, viz_scale}                ptr $ vizScale p
        #{poke Parameter, filter}                   ptr $ FCS.filter p
        #{poke Parameter, gain}                     ptr $ gain p
        #{poke Parameter, excitation_wavelengths}   ptr $ excitationWavelength p
        #{poke Parameter, excitation_power}         ptr $ excitationPower p
        #{poke Parameter, percent_light_collected}  ptr $ percentLightCollected p
        #{poke Parameter, name}                     ptr $ name p
        #{poke Parameter, detector_type}            ptr $ detectorType p
        #{poke Parameter, detector_voltage}         ptr $ detectorVoltage p
        #{poke Parameter, calibration}              ptr $ calibration p

instance Storable (Maybe Spillover) where
    sizeOf _ = #{size OptionalSpillover}
    alignment _ = #{alignment OptionalSpillover}
    poke ptr s = case s of
        Nothing -> do
            #{poke OptionalSpillover, present} ptr $ False
        Just s' -> do
            #{poke OptionalSpillover, present} ptr $ True
            #{poke OptionalSpillover, n_parameters} ptr $ nParams s'
            paramNameArray <- newArray $ parameterNames s'
            #{poke OptionalSpillover, parameters} ptr $ paramNameArray
            #{poke OptionalSpillover, matrix} ptr $ spilloverMatrix s'

instance Storable (Maybe Trigger) where
    sizeOf _ = #{size OptionalTrigger}
    alignment _ = #{alignment OptionalTrigger}
    poke ptr t = case t of 
        Nothing -> do
            #{poke OptionalTrigger, present} ptr $ False
        Just t' -> do
            #{poke OptionalTrigger, present} ptr $ True
            #{poke OptionalTrigger, trigger_channel} ptr $ triggerChannel t'
            #{poke OptionalTrigger, trigger_value} ptr $ triggerValue t'

instance Storable (T.Text, T.Text) where
    sizeOf _ = #{size MapItem}
    alignment _ = #{alignment MapItem}
    poke ptr kv = do
        #{poke MapItem, key} ptr $ fst kv
        #{poke MapItem, value} ptr $ snd kv

instance Storable (Map.Map T.Text T.Text) where
    sizeOf _ = #{size MapItems}
    alignment _ = #{alignment MapItems}
    poke ptr m
        | n_vals == 0 = do
            #{poke MapItems, n_vals} ptr $ n_vals
        | otherwise = do
            #{poke MapItems, n_vals} ptr $ n_vals
            array <- newArray $ Map.toList m :: IO (Ptr (T.Text, T.Text))
            #{poke MapItems, items} ptr $ array
        where n_vals = Map.size m


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
        #{poke FCSMetadata, extra_keyvals} ptr $ extraKeyvals m
        #{poke FCSMetadata, n_events_aborted} ptr $ nEventsAborted m
        #{poke FCSMetadata, acquire_time} ptr $ T.pack <$> iso8601Show <$> acquireTime m
        #{poke FCSMetadata, acquire_end_time} ptr $ T.pack <$> iso8601Show <$> acquireEndTime m
        #{poke FCSMetadata, acquire_date} ptr $ T.pack <$> iso8601Show <$> acquireDate m
        #{poke FCSMetadata, cells} ptr $ cells m
        #{poke FCSMetadata, comment} ptr $ comment m
        #{poke FCSMetadata, cytometer_type} ptr $ cytometerType m
        #{poke FCSMetadata, cytometer_serial_number} ptr $ cytometerSerialNumber m
        #{poke FCSMetadata, institution} ptr $ institution m
        #{poke FCSMetadata, experimenter} ptr $ experimenter m
        #{poke FCSMetadata, operator} ptr $ operator m
        #{poke FCSMetadata, filename} ptr $ filename m
        #{poke FCSMetadata, last_modified} ptr $ T.pack <$> iso8601Show <$> lastModified m
        #{poke FCSMetadata, last_modifier} ptr $ lastModifier m
        #{poke FCSMetadata, n_events_lost} ptr $ nEventsLost m
        #{poke FCSMetadata, originality} ptr $ originalityToEnum <$> originality m
        #{poke FCSMetadata, plate_id} ptr $ plateID m
        #{poke FCSMetadata, plate_name} ptr $ plateName m
        #{poke FCSMetadata, project} ptr $ project m
        #{poke FCSMetadata, specimen} ptr $ specimen m
        #{poke FCSMetadata, spillover} ptr $ spillover m
        #{poke FCSMetadata, specimen_source} ptr $ specimenSource m
        #{poke FCSMetadata, computer} ptr $ computer m
        #{poke FCSMetadata, timestep} ptr $ timestep m
        #{poke FCSMetadata, trigger} ptr $ trigger m
        #{poke FCSMetadata, well_id} ptr $ wellID m
        -- TODO
--  peek ptr

instance Storable FCS where
    sizeOf _ = #{size FCSFile}
    alignment _ = #{alignment FCSFile}
    poke ptr f = do
        #{poke FCSFile, name}          ptr $ T.pack "Test"
        #{poke FCSFile, metadata}      ptr $ metadata f
        #{poke FCSFile, uncompensated} ptr $ uncompensated $ dataSegment f
        #{poke FCSFile, compensated}   ptr $ compensated $ dataSegment f