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

sectorLength = 512

-- | Finds the padded size (in bytes) of the bitmap for a given block.
bitmapSizeOfBlock :: Block -> Int
bitmapSizeOfBlock (Block blockSize _) = bitmapSizeOfBlockSize blockSize

-- | Finds the padded size (in bytes) of the bitmap for a given block size.
bitmapSizeOfBlockSize :: BlockByteCount -> Int
bitmapSizeOfBlockSize blockSize = fromIntegral ((nbSector `divRoundUp` 8) `roundUpToModulo` sectorLength)
	where nbSector = blockSize `divRoundUp` sectorLength

-- | Retrieves the bitmap for the given block.
bitmapOfBlock :: Block -> Bitmap
bitmapOfBlock (Block _ ptr) = Bitmap ptr

-- | Finds the size (in bytes) of data stored within the block.
blockSizeOfBlock :: Block -> BlockByteCount
blockSizeOfBlock (Block bs _) = bs

-- | Retrieves the data for the given block.
dataOfBlock :: Block -> Data
dataOfBlock (Block bs ptr) = Data $ ptr `plusPtr` (bitmapSizeOfBlockSize bs)

-- | Obtains a direct pointer to the given data.
pointerOfData :: Data -> Ptr Word8
pointerOfData (Data ptr) = ptr

-- | Maps into memory a block of the given size, at the given file path and sector address.
withBlock :: FilePath -> BlockByteCount -> PhysicalSectorAddress -> (Block -> IO a) -> IO a
withBlock file blockSize sectorOffset f =
		mmapWithFilePtr file ReadWrite (Just (offset, length)) $ \(ptr, sz) ->
			f (Block blockSize $ castPtr ptr)
	where
		offset = (fromIntegral sectorOffset) * (fromIntegral sectorLength)
		length = (fromIntegral blockSize) + (fromIntegral $ bitmapSizeOfBlockSize blockSize)

-- | Reads into memory the contents of the bitmap for the specified block.
readBitmap :: Block -> IO ByteString
readBitmap block =
	B.create (fromIntegral length) create where
		length = bitmapSizeOfBlock block
		create byteStringPtr = B.memcpy target source (fromIntegral length) where
			source = case bitmapOfBlock block of Bitmap b -> b
			target = castPtr byteStringPtr

-- | Reads all available data from the specified block.
readData :: Block -> IO ByteString
readData block =
	readDataRange block 0 (fromIntegral $ blockSizeOfBlock block)

-- | Reads a range of data from within the specified block.
readDataRange :: Block -> BlockByteAddress -> BlockByteCount -> IO ByteString
readDataRange block offset length =
	B.create (fromIntegral length) (unsafeReadDataRange block offset length)

-- | Unsafely reads all available data from the specified block.
unsafeReadData :: Block -> Ptr Word8 -> IO ()
unsafeReadData block =
	unsafeReadDataRange block 0 (fromIntegral $ blockSizeOfBlock block)

-- | Unsafely reads a range of data from within the specified block.
unsafeReadDataRange :: Block -> BlockByteAddress -> BlockByteCount -> Ptr Word8 -> IO ()
unsafeReadDataRange block offset length target =
	B.memcpy target source (fromIntegral length)
	where
		source = (pointerOfData $ dataOfBlock block) `plusPtr` (fromIntegral offset)

-- | Writes data to the given byte address of the specified block.
writeDataRange :: Block -> BlockByteAddress -> ByteString -> IO ()
writeDataRange block offset content = do
	-- sectors need to be prepared for differential disk if the bitmap was clear before,
	-- at the moment assumption is it's 0ed
	bitmapSetRange bitmap (fromIntegral sectorStart) (fromIntegral sectorEnd)
	B.unsafeUseAsCString content (\source -> B.memcpy target (castPtr source) length)
	where
		length      = fromIntegral $ B.length content
		bitmap      = bitmapOfBlock block
		target      = (pointerOfData $ dataOfBlock block) `plusPtr` (fromIntegral offset)
		sectorStart = fromIntegral offset `div` sectorLength
		sectorEnd   = fromIntegral (fromIntegral offset + B.length content) `div` sectorLength
