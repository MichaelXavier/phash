{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PHash ( imageHash
                  , hammingDistance
                  , PHash(..) ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype PHash = PHash Word64 deriving (Show, Eq, Num)

imageHash :: FilePath -> IO (Maybe PHash)
imageHash path = withCString path $ \cs ->
  with startingPhash $ \pHPtr -> do
    res <- c_ph_dct_imagehash cs pHPtr
    if success res
      then Just . fromCPHash <$> peek pHPtr
      else return Nothing
  where startingPhash = CULong 0
        success (CInt (-1)) = False
        success _           = True

hammingDistance :: PHash -> PHash -> Int32
hammingDistance x y = unwrap $ c_ph_hamming_distance (toCPHash x) (toCPHash y)
  where unwrap (CInt i) = i

foreign import ccall "pHash.h ph_dct_imagehash" c_ph_dct_imagehash :: CString -> Ptr CULong -> IO CInt

foreign import ccall "pHash.h ph_hamming_distance" c_ph_hamming_distance :: CULong -> CULong -> CInt

toCPHash :: PHash -> CULong
toCPHash (PHash x) = CULong x

fromCPHash :: CULong -> PHash
fromCPHash (CULong x) = PHash x

