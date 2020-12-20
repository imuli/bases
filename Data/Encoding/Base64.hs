{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Data.Encoding.Base64
  ( base64Encode
  ) where

import           Data.Encoding.Endian
import           Data.Encoding.Fixed

import           Data.Bits (shiftL, unsafeShiftR, (.&.), (.|.))
import           Data.ByteString (ByteString, index)
import           Data.Vector.Unboxed (Vector, generate, unsafeIndex)
import           Data.Word (Word16, Word32, Word64, Word8)

import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (peek, poke)

base64 :: ByteString
base64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64t :: Vector Word16
base64t = generate 4096 $ \n ->
  let i = fromIntegral n
      y = i `unsafeShiftR` 6
      z = i .&. 0x3f
      a = fromIntegral $ base64 `index` y
      b = fromIntegral $ base64 `index` z
   in asBE $ a `shiftL` 8 .|. b

enc64 :: Ptr Word8 -> Ptr Word8 -> IO ()
enc64 !src !dst = do
  x <- asBE <$> peek @Word32 (castPtr src)
  let a' = x `unsafeShiftR` 20 .&. 0xfff
      b' = x `unsafeShiftR` 8 .&. 0xfff
      a = base64t `unsafeIndex` fromIntegral a'
      b = base64t `unsafeIndex` fromIntegral b'
  poke (castPtr dst) a
  poke (castPtr (plusPtr dst 2)) b

enc64q :: Ptr Word8 -> Ptr Word8 -> IO ()
enc64q !src !dst = do
  x <- asBE <$> peek @Word64 (castPtr src)
  let a' = x `unsafeShiftR` 52 .&. 0xfff
      b' = x `unsafeShiftR` 40 .&. 0xfff
      c' = x `unsafeShiftR` 28 .&. 0xfff
      d' = x `unsafeShiftR` 16 .&. 0xfff
      a = base64t `unsafeIndex` fromIntegral a'
      b = base64t `unsafeIndex` fromIntegral b'
      c = base64t `unsafeIndex` fromIntegral c'
      d = base64t `unsafeIndex` fromIntegral d'
  poke (castPtr dst) a
  poke (castPtr (plusPtr dst 2)) b
  poke (castPtr (plusPtr dst 4)) c
  poke (castPtr (plusPtr dst 6)) d

base64Encode :: ByteString -> ByteString
base64Encode bs = mapChunks 3 4 enc64 finNone bs
