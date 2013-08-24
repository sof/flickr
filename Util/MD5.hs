--------------------------------------------------------------------
-- |
-- Module    : Util.MD5
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: so-so
-- 
-- Haskell implementation of MD5, derived from RFC 1321.
-- 
module Util.MD5 
	( md5
	, md5sum
	, md5sumStr
	, showDigest
	) where

import Data.Bits
import Data.Word
import Data.Char ( ord, intToDigit )

type MD5Digest = (Word32, Word32, Word32, Word32)

md5sumStr :: [Char] -> String
md5sumStr xs = md5sum (map (fromIntegral.ord) xs)

md5sum :: [Word8] -> String
md5sum ls = showDigest $ md5 ls

md5 :: [Word8] -> [Word8]
md5 ls = splitUp $
             go 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0
                (chunk64 ls)
  where
   splitUp (a,b,c,d) = 
     wordToBytes a $ wordToBytes b $ wordToBytes c $ wordToBytes d []

   toWord32 :: [Word8] -> [Word32]
   toWord32 [] = []
   toWord32 (x:y:z:w:xs) = 
      ((fromIntegral x) .|.
       (fromIntegral y) `shiftL` 8 .|.
       (fromIntegral z) `shiftL` 16 .|.
       (fromIntegral w) `shiftL` 24) : toWord32 xs
   toWord32 xs = toWord32 (xs ++ replicate (4-length xs) 0)

   chunk64 cls = 
     case splitAt 64 cls of
       (as,[]) -> [as]
       (as,bs) -> as : chunk64 bs

   go :: Word32 -> Word32 -> Word32 -> Word32 -> Word64 -> [[Word8]]
      -> MD5Digest
   go a b c d _ []  = (a,b,c,d)
   go a b c d l [x] = 
     let
      l' = l + fromIntegral ((length x) `shiftL` 3)
      index = (fromIntegral ((l' `shiftR` 3) .&. 0x3f)) :: Word32
      padlen 
       | index < 56 =  56 - index
       | otherwise  = 120 - index
      

      l0 = (fromIntegral (l' .&. 0xffffffff)) :: Word32
      l1 = (fromIntegral ((l' `shiftR` 32) .&. 0xffffffff)) :: Word32

      len :: [Word8]
      len = wordToBytes l0 (wordToBytes l1 [])

      vs = (x ++ take (fromIntegral padlen) padding ++ len)
     in
     processRound a b c d (toWord32 vs)
   go a b c d l (x:xs) = 
     let
      (a1,b1,c1,d1) = processRound a b c d (toWord32 x)
      l1 = l + 64*8
     in
     go a1 b1 c1 d1 l1 xs

   processRound a0 b0 c0 d0 x = 
     let
      (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:xs) = x
      (a1,b1,c1,d1,_,_) = 
        round_4 $ round_3 $ round_2 $ round_1
	  (a0,b0,c0,d0,
	   [ x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15
	   , x1,x6,x11,x0,x5,x10,x15,x4,x9,x14,x3,x8,x13,x2,x7,x12
	   , x5,x8,x11,x14,x1,x4,x7,x10,x13,x0,x3,x6,x9,x12,x15,x2
	   , x0,x7,x14,x5,x12,x3,x10,x1,x8,x15,x6,x13,x4,x11,x2,x9
	   ],tab)
     in
     if not (null xs)
      then processRound (a0+a1) (b0+b1) (c0+c1) (d0+d1) xs
      else (a0+a1,b0+b1,c0+c1,d0+d1)

selByte :: Int -> Word32 -> Word8
selByte n x = fromIntegral ((x `shiftR` (8*n)) .&. 0xff)
      
wordToBytes :: Word32 -> [Word8] -> [Word8]
wordToBytes w ws = 
   (selByte 0 w) : (selByte 1 w) : 
   (selByte 2 w) : (selByte 3 w) : ws

type RoundR = (Word32,Word32,Word32,Word32,[Word32],[Word32])

type Round = RoundR -> RoundR

round_1 :: Round
round_1 = 
  (\ (a0,b0,c0,d0, 
      (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:xs),
      (t0:t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:t11:t12:t13:t14:t15:ts)) ->
     let 
      a1 = b0 + ((a0 + f b0 c0 d0 + x0 + t0) `rotateL`  7)
      d1 = a1 + ((d0 + f a1 b0 c0 + x1 + t1) `rotateL` 12)
      c1 = d1 + ((c0 + f d1 a1 b0 + x2 + t2) `rotateL` 17)
      b1 = c1 + ((b0 + f c1 d1 a1 + x3 + t3) `rotateL` 22)

      a2 = b1 + ((a1 + f b1 c1 d1 + x4 + t4) `rotateL`  7)
      d2 = a2 + ((d1 + f a2 b1 c1 + x5 + t5) `rotateL` 12)
      c2 = d2 + ((c1 + f d2 a2 b1 + x6 + t6) `rotateL` 17)
      b2 = c2 + ((b1 + f c2 d2 a2 + x7 + t7) `rotateL` 22)

      a3 = b2 + ((a2 + f b2 c2 d2 + x8 + t8) `rotateL`  7)
      d3 = a3 + ((d2 + f a3 b2 c2 + x9 + t9) `rotateL` 12)
      c3 = d3 + ((c2 + f d3 a3 b2 + x10 + t10) `rotateL` 17)
      b3 = c3 + ((b2 + f c3 d3 a3 + x11 + t11) `rotateL` 22)

      a4 = b3 + ((a3 + f b3 c3 d3 + x12 + t12) `rotateL`  7)
      d4 = a4 + ((d3 + f a4 b3 c3 + x13 + t13) `rotateL` 12)
      c4 = d4 + ((c3 + f d4 a4 b3 + x14 + t14) `rotateL` 17)
      b4 = c4 + ((b3 + f c4 d4 a4 + x15 + t15) `rotateL` 22)
     in
     (a4, b4, c4, d4, xs, ts))

round_2 :: Round
round_2 = 
  (\ (a0,b0,c0,d0, 
      (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:xs),
      (t0:t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:t11:t12:t13:t14:t15:ts)) ->
     let
      a1 = b0 + ((a0 + g b0 c0 d0 + x0 + t0) `rotateL` 5)
      d1 = a1 + ((d0 + g a1 b0 c0 + x1 + t1) `rotateL` 9)
      c1 = d1 + ((c0 + g d1 a1 b0 + x2 + t2) `rotateL` 14)
      b1 = c1 + ((b0 + g c1 d1 a1 + x3 + t3) `rotateL` 20)

      a2 = b1 + ((a1 + g b1 c1 d1 + x4 + t4) `rotateL`  5)
      d2 = a2 + ((d1 + g a2 b1 c1 + x5 + t5) `rotateL`  9)
      c2 = d2 + ((c1 + g d2 a2 b1 + x6 + t6) `rotateL` 14)
      b2 = c2 + ((b1 + g c2 d2 a2 + x7 + t7) `rotateL` 20)

      a3 = b2 + ((a2 + g b2 c2 d2 + x8 + t8) `rotateL`  5)
      d3 = a3 + ((d2 + g a3 b2 c2 + x9 + t9) `rotateL`  9)
      c3 = d3 + ((c2 + g d3 a3 b2 + x10 + t10) `rotateL` 14)
      b3 = c3 + ((b2 + g c3 d3 a3 + x11 + t11) `rotateL` 20)

      a4 = b3 + ((a3 + g b3 c3 d3 + x12 + t12) `rotateL`  5)
      d4 = a4 + ((d3 + g a4 b3 c3 + x13 + t13) `rotateL`  9)
      c4 = d4 + ((c3 + g d4 a4 b3 + x14 + t14) `rotateL` 14)
      b4 = c4 + ((b3 + g c4 d4 a4 + x15 + t15) `rotateL` 20)
     in
     (a4, b4, c4, d4, xs, ts))

round_3 :: Round
round_3 = 
  (\ (a0,b0,c0,d0, 
      (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:xs),
      (t0:t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:t11:t12:t13:t14:t15:ts)) ->
     let
      a1 = b0 + ((a0 + h b0 c0 d0 + x0 + t0) `rotateL` 4)
      d1 = a1 + ((d0 + h a1 b0 c0 + x1 + t1) `rotateL` 11)
      c1 = d1 + ((c0 + h d1 a1 b0 + x2 + t2) `rotateL` 16)
      b1 = c1 + ((b0 + h c1 d1 a1 + x3 + t3) `rotateL` 23)

      a2 = b1 + ((a1 + h b1 c1 d1 + x4 + t4) `rotateL`  4)
      d2 = a2 + ((d1 + h a2 b1 c1 + x5 + t5) `rotateL` 11)
      c2 = d2 + ((c1 + h d2 a2 b1 + x6 + t6) `rotateL` 16)
      b2 = c2 + ((b1 + h c2 d2 a2 + x7 + t7) `rotateL` 23)

      a3 = b2 + ((a2 + h b2 c2 d2 + x8 + t8) `rotateL`  4)
      d3 = a3 + ((d2 + h a3 b2 c2 + x9 + t9) `rotateL`  11)
      c3 = d3 + ((c2 + h d3 a3 b2 + x10 + t10) `rotateL` 16)
      b3 = c3 + ((b2 + h c3 d3 a3 + x11 + t11) `rotateL` 23)

      a4 = b3 + ((a3 + h b3 c3 d3 + x12 + t12) `rotateL`  4)
      d4 = a4 + ((d3 + h a4 b3 c3 + x13 + t13) `rotateL` 11)
      c4 = d4 + ((c3 + h d4 a4 b3 + x14 + t14) `rotateL` 16)
      b4 = c4 + ((b3 + h c4 d4 a4 + x15 + t15) `rotateL` 23)
     in
     (a4, b4, c4, d4, xs, ts))

round_4 :: Round
round_4 = 
  (\ (a0,b0,c0,d0, 
      (x0:x1:x2:x3:x4:x5:x6:x7:x8:x9:x10:x11:x12:x13:x14:x15:xs),
      (t0:t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:t11:t12:t13:t14:t15:ts)) ->
     let
      a1 = b0 + ((a0 + i b0 c0 d0 + x0 + t0) `rotateL` 6)
      d1 = a1 + ((d0 + i a1 b0 c0 + x1 + t1) `rotateL` 10)
      c1 = d1 + ((c0 + i d1 a1 b0 + x2 + t2) `rotateL` 15)
      b1 = c1 + ((b0 + i c1 d1 a1 + x3 + t3) `rotateL` 21)

      a2 = b1 + ((a1 + i b1 c1 d1 + x4 + t4) `rotateL`  6)
      d2 = a2 + ((d1 + i a2 b1 c1 + x5 + t5) `rotateL` 10)
      c2 = d2 + ((c1 + i d2 a2 b1 + x6 + t6) `rotateL` 15)
      b2 = c2 + ((b1 + i c2 d2 a2 + x7 + t7) `rotateL` 21)

      a3 = b2 + ((a2 + i b2 c2 d2 + x8 + t8) `rotateL`  6)
      d3 = a3 + ((d2 + i a3 b2 c2 + x9 + t9) `rotateL`  10)
      c3 = d3 + ((c2 + i d3 a3 b2 + x10 + t10) `rotateL` 15)
      b3 = c3 + ((b2 + i c3 d3 a3 + x11 + t11) `rotateL` 21)

      a4 = b3 + ((a3 + i b3 c3 d3 + x12 + t12) `rotateL`  6)
      d4 = a4 + ((d3 + i a4 b3 c3 + x13 + t13) `rotateL` 10)
      c4 = d4 + ((c3 + i d4 a4 b3 + x14 + t14) `rotateL` 15)
      b4 = c4 + ((b3 + i c4 d4 a4 + x15 + t15) `rotateL` 21)
     in
     (a4, b4, c4, d4, xs, ts))


f,g,h,i :: Word32 -> Word32 -> Word32 -> Word32
f x y z = (x .&. y) .|. (complement x .&. z)
g x y z = (x .&. z) .|. (y .&. complement z)
h x y z = (x `xor` y `xor` z)
i x y z = (y `xor` (x .|. complement z))

padding :: [Word8]
padding = (0x80:replicate 63 0x00)

-- [ (floor ((4294967296.0::Double) * abs (sin (fromIntegral i)))) :: Integer 
--      | i <- [1..(64::Int)] ]
tab :: [Word32]
tab =
 [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
 , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
 , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
 , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
 
 , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
 , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
 , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
 , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a

 , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
 , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
 , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
 , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665

 , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
 , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
 , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
 , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
 ]

showDigest :: [Word8] -> String
showDigest ds = foldr showHex "" ds
 where
  showHex :: Word8 -> String -> String
  showHex x xs = (hdig a):(hdig b):xs
   where
    (a,b) = x `divMod` 16
    
    hdig hd = intToDigit (fromIntegral hd)

