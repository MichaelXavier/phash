{-# LANGUAGE ForeignFunctionInterface #-}
module Data.PHash ( hammingDistance
                  , module Data.PHash.Image
                  , module Data.PHash.Types
                  , module Data.PHash.Video ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.PHash.Image
import Data.PHash.Types
import Data.PHash.Video

hammingDistance :: PHash -> PHash -> Int32
hammingDistance x y = unwrap $ c_ph_hamming_distance (toCPHash x) (toCPHash y)
  where unwrap (CInt i) = i

foreign import ccall "pHash.h ph_hamming_distance" c_ph_hamming_distance :: CULong -> CULong -> CInt
