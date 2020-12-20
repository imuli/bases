{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Encoding.Base16
  ( base16Encode
  ) where

import Data.Encoding.Fixed
import Data.Encoding.Endian

import Data.Bits (shiftL, (.|.), (.&.), unsafeShiftR)
import Data.Word (Word32, Word16, Word8)
import Data.ByteString (ByteString, index)
import Data.Vector.Unboxed (Vector, generate, unsafeIndex)

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)

base16 :: ByteString
base16 = "0123456789abcdef"

base16w :: Vector Word32
base16w = generate 65536 $ \n ->
  let i = fromIntegral $ asBE @Word16 $ fromIntegral n
      z = i .&. 0xf
      y = i `unsafeShiftR` 4 .&. 0xf
      x = i `unsafeShiftR` 8 .&. 0xf
      w = i `unsafeShiftR` 12
      a = fromIntegral $ base16 `index` w
      b = fromIntegral $ base16 `index` x
      c = fromIntegral $ base16 `index` y
      d = fromIntegral $ base16 `index` z
   in asBE $ a `shiftL` 24 .|. b `shiftL` 16 .|. c `shiftL` 8 .|. d

enc16 :: Ptr Word8 -> Ptr Word8 -> IO ()
enc16 !src !dst = do
  x <- peek @Word8 src
  let !h = x `unsafeShiftR` 4
      !l = x .&. 0xf
  poke dst (base16 `index` fromIntegral h)
  poke (plusPtr dst 1) (base16 `index` fromIntegral l)
{-# INLINE enc16 #-}

enc16w :: Ptr Word8 -> Ptr Word8 -> IO ()
enc16w !src !dst = do
  x <- peek @Word16 (castPtr src)
  poke (castPtr dst) (base16w `unsafeIndex` fromIntegral x)
{-# INLINE enc16w #-}

fin16w :: Int -> Int -> Ptr Word8 -> Ptr Word8 -> IO Int
fin16w !n 0 _ _ = pure (2*n)
fin16w !n !x !src !dst = enc16 src dst *> fin16w (n+1) (x-1) (plusPtr src 1) (plusPtr dst 2)

base16Encode :: ByteString -> ByteString
base16Encode bs = mapChunks 2 4 enc16w (fin16w 0) bs
