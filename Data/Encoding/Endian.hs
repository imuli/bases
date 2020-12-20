{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}

module Data.Encoding.Endian where

import Data.Word (Word64, Word32, Word16, byteSwap64, byteSwap32, byteSwap16)

class Endian a where
  asBE :: a -> a
  asLE :: a -> a

instance Endian Word64 where
#ifdef WORDS_BIGENDIAN
  asBE = id
  asLE = byteSwap64
#else
  asBE = byteSwap64
  asLE = id
#endif

instance Endian Word32 where
#ifdef WORDS_BIGENDIAN
  asBE = id
  asLE = byteSwap32
#else
  asBE = byteSwap32
  asLE = id
#endif

instance Endian Word16 where
#ifdef WORDS_BIGENDIAN
  asBE = id
  asLE = byteSwap16
#else
  asBE = byteSwap16
  asLE = id
#endif

