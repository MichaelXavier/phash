{-# LANGUAGE ForeignFunctionInterface #-}
module Data.PHash.Image ( imageHash ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.PHash.Types

-- $setup
-- >>> let imgPath = "test/fixtures/grump.jpg"
-- >>> let bogusPath = "bogus"

{-|
Obtain the hash of an image. Returns Nothing on failure. pHash's API does
not provide any error information when this fails, but CImg may dump
something to stderr.

Examples:

>>> import Data.PHash
>>> imageHash imgPath
Just (PHash 17549625427362946731)
-}
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

foreign import ccall "pHash.h ph_dct_imagehash" c_ph_dct_imagehash :: CString -> Ptr CULong -> IO CInt
