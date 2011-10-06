module Data.VHD.Block
	( Block
	, Sector
	, withBlock
	, writeBlock
	, bitmapSizeOfBlock
	, bitmapOfBlock
	, sectorLength
	, Bitmap (..)
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

data Block = Block BlockSize (Ptr Word8)
data Bitmap = Bitmap (Ptr Word8)
data Sector = Sector (Ptr Word8)

sectorLength :: Word32
sectorLength = 512

-- | this is the padded size of the bitmap for a specific blocksize
bitmapSizeOfBlock :: BlockSize -> Int
bitmapSizeOfBlock blockSize = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
	where nbSector = blockSize `divRoundUp` sectorLength

-- | get a bitmap type out of a block type.
-- the bitmap happens to be at the beginning of the block.
bitmapOfBlock :: Block -> Bitmap
bitmapOfBlock (Block _ ptr) = Bitmap ptr

dataOfBlock :: Block -> Ptr Word8
dataOfBlock (Block bs ptr) = ptr `plusPtr` (bitmapSizeOfBlock bs)

-- | mmap a block using a filepath, a blocksize
withBlock :: FilePath -> BlockSize -> Word32 -> (Block -> IO a) -> IO a
withBlock file bs sectorOff f = mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
		f (Block bs $ castPtr ptr)
	where
		absoluteOffset = fromIntegral (fromIntegral sectorOff * sectorLength)
		offsetSize     = (absoluteOffset, fromIntegral bs + fromIntegral bitmapSize)
		nbSector       = (bs `divRoundUp` sectorLength)
		bitmapSize     = (nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength

writeBlock :: Block -> ByteString -> Int -> IO ()
writeBlock block content offset = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmapPtr (fromIntegral sectorStart) (fromIntegral sectorEnd)
	B.unsafeUseAsCString content (\bsptr -> B.memcpy (dataPtr `plusPtr` offset) (castPtr bsptr) (fromIntegral $ B.length content))
	where
		bitmapPtr   = bitmapOfBlock block
		dataPtr     = dataOfBlock block
		sectorStart = fromIntegral offset `div` sectorLength
		sectorEnd   = fromIntegral (offset + B.length content) `div` sectorLength

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

bitmapSetRange :: Bitmap -> Int -> Int -> IO ()
bitmapSetRange bitmap start end
	| start < end = bitmapSet bitmap start >> bitmapSetRange bitmap (start + 1) end
	| otherwise   = return ()

bitmapClear :: Bitmap -> Int -> IO ()
bitmapClear bitmap n = bitmapModify bitmap n (flip clearBit)
