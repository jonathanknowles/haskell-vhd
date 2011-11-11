module Data.Vhd.Bitmap
	( Bitmap (..)
	, bitmapGet
	, bitmapSet
	, bitmapSetRange
	, bitmapClear
	) where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable

data Bitmap = Bitmap (Ptr Word8)

bitmapGet :: Bitmap -> Int -> IO Bool
bitmapGet (Bitmap ptr) n = test `fmap` peekByteOff ptr offset
	where
		test :: Word8 -> Bool
		test = flip testBit (7 - bit)
		(offset, bit) = n `divMod` 8

bitmapModify :: Bitmap -> Int -> (Int -> Word8 -> Word8) -> IO ()
bitmapModify (Bitmap bptr) n f = peek ptr >>= poke ptr . f (7 - bit)
	where
		ptr = bptr `plusPtr` offset
		(offset, bit) = n `divMod` 8

bitmapSet :: Bitmap -> Int -> IO ()
bitmapSet bitmap n = bitmapModify bitmap n (flip setBit)

bitmapSetRange :: Bitmap -> Int -> Int -> IO ()
bitmapSetRange bitmap start end
	| start < end = bitmapSet bitmap start >> bitmapSetRange bitmap (start + 1) end
	| otherwise   = return ()

bitmapClear :: Bitmap -> Int -> IO ()
bitmapClear bitmap n = bitmapModify bitmap n (flip clearBit)
