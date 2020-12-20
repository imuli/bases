{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module       : Data.Encoding.Fixed
-- License      : Unlicense (Public Domain dedication)
--
-- Maintainer   : Imuli <i@imu.li>
-- Stability    : Experimental
-- Portability  : portable
--
-- Helpers for fixed entropy encodings.
--

module Data.Encoding.Fixed
  where

import Data.Word (Word8)
import Data.ByteString.Internal (ByteString(..), unsafeCreateUptoN)
import Foreign.Ptr (Ptr, castPtr, plusPtr, minusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (Storable(..))

import Control.Exception (SomeException, try)
import System.IO.Unsafe (unsafePerformIO) 

-- | Types backed by flat arrays in memory, such that we can peek and poke at the memory inside of them.
class FlatArray a where
  byteCount :: a -> Int
  withStartPtr :: a -> (Ptr Word8 -> IO c) -> IO c
  unsafeAlloc :: Int -> (Ptr Word8 -> IO Int) -> a

instance FlatArray ByteString where
  byteCount (PS _ _ !len) = len
  withStartPtr (PS !fp !off _) f = withForeignPtr fp $ \p -> f (plusPtr p off)
  unsafeAlloc = unsafeCreateUptoN

-- | Transform known sized chunks of memory to differently sized chunks of memory.
--
-- > mapChunks inChunk outChunk flow finish source
--
-- transforms @source@ via @flow@, stepping it's input @inChunk@ bytes at a
-- time and it's output @outChunk@ bytes at a time, calling @finish@ to handle
-- any extra bytes of input left over at the end.

mapChunks :: (FlatArray a, FlatArray b, Storable i, Storable o, Storable i', Storable o') =>
  Int -> Int -> (Ptr i -> Ptr o -> IO ()) -> (Int -> Ptr i' -> Ptr o' -> IO Int) -> a -> b
mapChunks !inChunk !outChunk flow finish source = 
  let !slen = byteCount source
      !dlen = ((slen + inChunk - 1) `div` inChunk) * outChunk
   in unsafeAlloc dlen $ \dptr ->
     withStartPtr source $ \sptr ->
       let !stop = plusPtr sptr slen
           loop !src !dst
             | plusPtr src inChunk > stop =
               (minusPtr dst dptr +) <$> finish (minusPtr stop src) (castPtr src) (castPtr dst)
             | otherwise = do
               flow (castPtr src) (castPtr dst)
               loop (plusPtr src inChunk) (plusPtr dst outChunk)
        in loop sptr dptr
{-# INLINE mapChunks #-}

attempt :: (a -> b) -> a -> Either String b
attempt f x = case unsafePerformIO $ try $ pure $ f x of
                   Left e -> Left (show (e :: SomeException))
                   Right d -> Right d

-- | A finish routine that always succeeds and add no output.

finNone :: Int -> Ptr Word8 -> Ptr Word8 -> IO Int
finNone _ _ _ = pure 0

