{-# LANGUAGE ForeignFunctionInterface #-}
module Data.PHash ( hammingDistance
                  , imagesSimilar
                  , module Data.PHash.Image
                  , module Data.PHash.Types ) where

import Control.Applicative ( (<$>)
                           , (<*>) )
import Foreign.C.Types

import Data.PHash.Image
import Data.PHash.Types

hammingDistance :: PHash -> PHash -> Int
hammingDistance x y = unwrap $ c_ph_hamming_distance (toCPHash x) (toCPHash y)
  where unwrap (CInt i) = fromIntegral i

imagesSimilar :: FilePath -> FilePath -> Int -> IO (Maybe Bool)
imagesSimilar p1 p2 threshold = do
  h1 <- imageHash p1
  h2 <- imageHash p2
  return $ checkDistance <$> h1 <*> h2
  where checkDistance h1 h2 = (<=threshold) $ hammingDistance h1 h2

foreign import ccall "pHash.h ph_hamming_distance" c_ph_hamming_distance :: CULong -> CULong -> CInt
