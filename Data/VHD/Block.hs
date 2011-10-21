module Data.VHD.Block
	( Block
	, Sector
	, bitmapSizeOfBlock
	, bitmapOfBlock
	, withBlock
	, readBlock
	, writeBlock
	, sectorLength
	) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Bits

import Data.VHD.Utils
import Data.VHD.Types
import Data.VHD.Bat
import Data.VHD.Bitmap

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

import Control.Monad

import System.IO.MMap

data Block = Block BlockSize (Ptr Word8)
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

readBlock :: Block -> Int -> Int -> IO ByteString
readBlock block offsetStart offsetEnd = do
	-- for the moment, assume we're reading from a dynamic disk.
	-- later on, we'll also need to handle differencing disks here.
	B.create length (\bsptr -> B.memcpy (castPtr bsptr) (dataPtr `plusPtr` offsetStart) (fromIntegral length))
	where
		length      = offsetEnd - offsetStart
		bitmapPtr   = bitmapOfBlock block
		dataPtr     = dataOfBlock block
		sectorStart = fromIntegral offsetStart `div` sectorLength
		sectorEnd   = fromIntegral offsetEnd   `div` sectorLength

writeBlock :: Block -> ByteString -> Int -> IO ()
writeBlock block content offset = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmapPtr (fromIntegral sectorStart) (fromIntegral sectorEnd)
	B.unsafeUseAsCString content (\bsptr -> B.memcpy (dataPtr `plusPtr` offset) (castPtr bsptr) (fromIntegral length))
	where
		length      = B.length content
		bitmapPtr   = bitmapOfBlock block
		dataPtr     = dataOfBlock block
		sectorStart = fromIntegral offset `div` sectorLength
		sectorEnd   = fromIntegral (offset + B.length content) `div` sectorLength
