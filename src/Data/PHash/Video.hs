-- ulong64* ph_dct_videohash(const char *file, int &Length);
{-# LANGUAGE ForeignFunctionInterface #-}
module Data.PHash.Video ( videoHash ) where

import Control.Applicative ((<$>))
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.PHash.Types

videoHash :: FilePath -> IO [PHash]
videoHash path = withCString path $ \cs -> do
  with startingLen $ \lenPtr -> do
    arrPtr <- c_ph_dct_videohash cs lenPtr
    putStrLn "GOT ARRPTR"
    arrLen <- fromIntegral <$> peek lenPtr
    putStrLn "GOT ARRLEN"
    hashes <- peekArray arrLen arrPtr
    putStrLn "GOT HASHES"
    -- deallocArray arrPtr arrLen
    return . map fromCPHash $ hashes
  where startingLen = CInt 0

foreign import ccall "pHash.h ph_dct_videohash" c_ph_dct_videohash :: CString -> Ptr CInt -> IO (Ptr CULong)

deallocArray :: Ptr a -> Int -> IO ()
deallocArray arrHead arrLen = mapM_ deallocSlot slots
  where slots       = [0..arrLen-1]
        deallocSlot = free . plusPtr arrHead
