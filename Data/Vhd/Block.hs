module Data.Vhd.Block
	( Block
	, Sector
	, bitmapSizeOfBlockSize
	, bitmapOfBlock
	, withBlock
	, readBitmap
	, readData
	, readDataRange
	, writeDataRange
	, sectorLength
	) where

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import Data.Bits

import Data.Vhd.Utils
import Data.Vhd.Types
import Data.Vhd.Bat
import Data.Vhd.Bitmap

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

import Control.Monad

import System.IO.MMap

data Block = Block BlockSize (Ptr Word8)
data Sector = Sector (Ptr Word8)
data Data = Data (Ptr Word8)

sectorLength :: Word32
sectorLength = 512

bitmapSizeOfBlock :: Block -> Int
bitmapSizeOfBlock (Block blockSize _) = bitmapSizeOfBlockSize blockSize

-- | this is the padded size of the bitmap for a specific blocksize
bitmapSizeOfBlockSize :: BlockSize -> Int
bitmapSizeOfBlockSize blockSize = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
	where nbSector = blockSize `divRoundUp` sectorLength

-- | get a bitmap type out of a block type.
-- the bitmap happens to be at the beginning of the block.
bitmapOfBlock :: Block -> Bitmap
bitmapOfBlock (Block _ ptr) = Bitmap ptr

blockSizeOfBlock :: Block -> BlockSize
blockSizeOfBlock (Block bs _) = bs

dataOfBlock :: Block -> Data
dataOfBlock (Block bs ptr) = Data $ ptr `plusPtr` (bitmapSizeOfBlockSize bs)

-- | mmap a block using a filepath, a blocksize
withBlock :: FilePath -> BlockSize -> Word32 -> (Block -> IO a) -> IO a
withBlock file blockSize sectorOffset f =
		mmapWithFilePtr file ReadWrite (Just (offset, length)) $ \(ptr, sz) ->
			f (Block blockSize $ castPtr ptr)
	where
		offset = (fromIntegral sectorOffset) * (fromIntegral sectorLength)
		length = (fromIntegral blockSize) + (fromIntegral $ bitmapSizeOfBlockSize blockSize)

readBitmap :: Block -> IO ByteString
readBitmap block =
	B.create (fromIntegral length) create where
		length = bitmapSizeOfBlock block
		create byteStringPtr = B.memcpy target source (fromIntegral length) where
			source = case bitmapOfBlock block of Bitmap b -> b
			target = castPtr byteStringPtr

readData :: Block -> IO ByteString
readData block = readDataRange block 0 (fromIntegral $ blockSizeOfBlock block)

readDataRange :: Block -> Int -> Int -> IO ByteString
readDataRange block offsetStart offsetEnd = do
	-- for the moment, assume we're reading from a dynamic disk.
	-- later on, we'll also need to handle differencing disks here.
	B.create length (\bsptr -> B.memcpy (castPtr bsptr) (dataPtr `plusPtr` offsetStart) (fromIntegral length))
	where
		length       = offsetEnd - offsetStart
		Data dataPtr = dataOfBlock block
		sectorStart  = fromIntegral offsetStart `div` sectorLength
		sectorEnd    = fromIntegral offsetEnd   `div` sectorLength

writeDataRange :: Block -> ByteString -> Int -> IO ()
writeDataRange block content offset = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmap (fromIntegral sectorStart) (fromIntegral sectorEnd)
	B.unsafeUseAsCString content (\bsptr -> B.memcpy (dataPtr `plusPtr` offset) (castPtr bsptr) (fromIntegral length))
	where
		length       = B.length content
		bitmap       = bitmapOfBlock block
		Data dataPtr = dataOfBlock block
		sectorStart  = fromIntegral offset `div` sectorLength
		sectorEnd    = fromIntegral (offset + B.length content) `div` sectorLength
