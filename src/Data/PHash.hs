{-# LANGUAGE ForeignFunctionInterface #-}
module Data.PHash ( hammingDistance
                  , imagesSimilar
                  , module Data.PHash.Image
                  , module Data.PHash.Types ) where

import           Control.Applicative
import           Foreign.C.Types

import           Prelude

import           Data.PHash.Image
import           Data.PHash.Types

{-|
Calculate the distance between two hashes. This can be used to detect how
similar two images are.

>>> import Data.PHash
>>> hammingDistance (PHash 15243782418149777067) (PHash 17549625427362946731)
2

>>> hammingDistance (PHash 15243782418149777067) (PHash 15243782418149777067)
0
-}
hammingDistance :: PHash -> PHash -> Int
hammingDistance x y = unwrap $ c_ph_hamming_distance (toCPHash x) (toCPHash y)
  where unwrap (CInt i) = fromIntegral i

-- |Determine if two images are similar by a user-defined threshold
imagesSimilar :: FilePath
              -> FilePath
              -> Int -- ^ Threshold for similarity. If the hamming distance exceeds this number, it will return False. 15 seems to be a reasonable default.
              -> IO (Maybe Bool)
imagesSimilar p1 p2 threshold = do
  h1 <- imageHash p1
  h2 <- imageHash p2
  return $ checkDistance <$> h1 <*> h2
  where checkDistance h1 h2 = (<=threshold) $ hammingDistance h1 h2

foreign import ccall "pHash.h ph_hamming_distance" c_ph_hamming_distance :: CULong -> CULong -> CInt

