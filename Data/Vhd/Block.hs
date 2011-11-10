module Data.Vhd.Block
	( Block
	, Sector
	, bitmapSizeOfBlockSize
	, bitmapOfBlock
	, withBlock
	, readBitmap
	, readData
	, readDataRange
	, unsafeReadData
	, unsafeReadDataRange
	, writeDataRange
	, sectorLength
	) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import Data.Vhd.Bat
import Data.Vhd.Bitmap
import Data.Vhd.Types
import Data.Vhd.Utils
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.IO.MMap

data Block = Block BlockByteCount (Ptr Word8)
data Sector = Sector (Ptr Word8)
data Data = Data (Ptr Word8)

sectorLength :: Word32
sectorLength = 512

bitmapSizeOfBlock :: Block -> Int
bitmapSizeOfBlock (Block blockSize _) = bitmapSizeOfBlockSize blockSize

-- | this is the padded size of the bitmap for a specific blocksize
bitmapSizeOfBlockSize :: BlockByteCount -> Int
bitmapSizeOfBlockSize blockSize = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
	where nbSector = blockSize `divRoundUp` sectorLength

-- | get a bitmap type out of a block type.
-- the bitmap happens to be at the beginning of the block.
bitmapOfBlock :: Block -> Bitmap
bitmapOfBlock (Block _ ptr) = Bitmap ptr

blockSizeOfBlock :: Block -> BlockByteCount
blockSizeOfBlock (Block bs _) = bs

dataOfBlock :: Block -> Data
dataOfBlock (Block bs ptr) = Data $ ptr `plusPtr` (bitmapSizeOfBlockSize bs)

pointerOfData :: Data -> Ptr Word8
pointerOfData (Data ptr) = ptr

-- | mmap a block using a filepath, a blocksize
withBlock :: FilePath -> BlockByteCount -> PhysicalSectorAddress -> (Block -> IO a) -> IO a
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
readData block =
	readDataRange block 0 (fromIntegral $ blockSizeOfBlock block)

readDataRange :: Block -> BlockByteAddress -> BlockByteCount -> IO ByteString
readDataRange block offset length =
	B.create (fromIntegral length) (unsafeReadDataRange block offset length)

unsafeReadData :: Block -> Ptr Word8 -> IO ()
unsafeReadData block =
	unsafeReadDataRange block 0 (fromIntegral $ blockSizeOfBlock block)

unsafeReadDataRange :: Block -> BlockByteAddress -> BlockByteCount -> Ptr Word8 -> IO ()
unsafeReadDataRange block offset length target =
	B.memcpy target source (fromIntegral length)
	where
		source = (pointerOfData $ dataOfBlock block) `plusPtr` (fromIntegral offset)

writeDataRange :: Block -> Int -> ByteString -> IO ()
writeDataRange block offset content = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmap (fromIntegral sectorStart) (fromIntegral sectorEnd)
	B.unsafeUseAsCString content (\source -> B.memcpy target (castPtr source) length)
	where
		length      = fromIntegral $ B.length content
		bitmap      = bitmapOfBlock block
		target      = (pointerOfData $ dataOfBlock block) `plusPtr` offset
		sectorStart = fromIntegral offset `div` sectorLength
		sectorEnd   = fromIntegral (offset + B.length content) `div` sectorLength
