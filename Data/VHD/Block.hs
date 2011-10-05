module Data.VHD.Block
	( BlockPtr
	, Sector
	, withBlock
	, writeBlock
	, bitmapSizeOfBlock
	, bitmapOfBlock
	, sectorLength
	, Bitmap(..)
	, bitmapGet
	, bitmapSet
	, bitmapClear
	) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Bits

import Data.VHD.Utils
import Data.VHD.Types
import Data.VHD.Bat

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

import Control.Monad

import System.IO.MMap

data BlockPtr = BlockPtr BlockSize (Ptr Word8)

data Sector = Sector (Ptr Word8)

sectorLength :: Word32
sectorLength = 512

-- | this is the padded size of the bitmap for a specific blocksize
bitmapSizeOfBlock :: BlockSize -> Int
bitmapSizeOfBlock blockSize = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
	where nbSector = blockSize `divRoundUp` sectorLength

-- | get a bitmap type out of a block type.
-- the bitmap happens to be at the beginning of the block.
bitmapOfBlock :: BlockPtr -> Bitmap
bitmapOfBlock (BlockPtr _ ptr) = Bitmap ptr

dataOfBlock :: BlockPtr -> Ptr Word8
dataOfBlock (BlockPtr bs ptr) = ptr `plusPtr` (bitmapSizeOfBlock bs)

-- | mmap a block using a filepath, a blocksize
withBlock :: FilePath -> BlockSize -> Word32 -> (BlockPtr -> IO a) -> IO a
withBlock file bs sectorOff f = mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
		f (BlockPtr bs $ castPtr ptr)
	where
		absoluteOffset = fromIntegral (fromIntegral sectorOff * sectorLength)
		offsetSize     = (absoluteOffset, fromIntegral bs + fromIntegral bitmapSize)
		nbSector       = (bs `divRoundUp` sectorLength)
		bitmapSize     = (nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength


writeBlock :: BlockPtr -> ByteString -> Int -> IO ()
writeBlock block bs offset = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmap (fromIntegral bsector) (fromIntegral $ esector-1)
	B.unsafeUseAsCString bs (\bsptr -> B.memcpy (dataPtr `plusPtr` offset) (castPtr bsptr) (fromIntegral $ B.length bs))
	where
		bitmap  = bitmapOfBlock block
		dataPtr = dataOfBlock block
		bsector = fromIntegral offset `div` sectorLength
		esector = fromIntegral (offset + B.length bs) `div` sectorLength

data Bitmap = Bitmap (Ptr Word8)

bitmapGet :: Bitmap -> Int -> IO Bool
bitmapGet (Bitmap ptr) n = test `fmap` peekByteOff ptr offset
	where
		test :: Word8 -> Bool
		test = flip testBit (7-bit)
		(offset, bit) = n `divMod` 8

bitmapModify :: Bitmap -> Int -> (Int -> Word8 -> Word8) -> IO ()
bitmapModify (Bitmap bptr) n f = peek ptr >>= poke ptr . f (7-bit)
	where
		ptr = bptr `plusPtr` offset
		(offset, bit) = n `divMod` 8

bitmapSet :: Bitmap -> Int -> IO ()
bitmapSet bitmap n = bitmapModify bitmap n (flip setBit)

-- FIXME use a faster way to set multiple bits when time permits
bitmapSetRange :: Bitmap -> Int -> Int -> IO ()
bitmapSetRange bitmap s e
	| s == e    = bitmapSet bitmap s
	| s < e     = bitmapSet bitmap s >> bitmapSetRange bitmap (s+1) e
	| otherwise = return ()

bitmapClear :: Bitmap -> Int -> IO ()
bitmapClear bitmap n = bitmapModify bitmap n (flip clearBit)
