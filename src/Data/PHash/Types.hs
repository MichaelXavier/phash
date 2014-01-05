{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.PHash.Types ( PHash(..)
                        , toCPHash
                        , fromCPHash ) where

import Data.Word  ( Word64 )
import Foreign.C.Types ( CULong(..) )

newtype PHash = PHash Word64 deriving (Show, Eq, Num)

toCPHash :: PHash -> CULong
toCPHash (PHash x) = CULong x

fromCPHash :: CULong -> PHash
fromCPHash (CULong x) = PHash x

