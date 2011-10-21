module Data.VHD.Bat
	( Bat (..)
	, batGetSize
	, batRead
	, batWrite
	, batMmap
	, batIterate
	, batUpdateChecksum
	) where

import Data.Word
import Data.Bits
import Data.Storable.Endian
import Foreign.Ptr
import Foreign.Storable

import Data.VHD.Types
import Data.VHD.Serialize
import Data.VHD.Utils
import Data.VHD.Bitmap

import Control.Monad

import System.IO.MMap

data Batmap = Batmap Bitmap Int
data Bat = Bat (Ptr Word32) (Ptr Word32) (Maybe Batmap)

sectorLength = 512

batmapSet n (Batmap bitmap _) = bitmapSet bitmap n

batmapChecksum :: Batmap -> IO CheckSum
batmapChecksum (Batmap (Bitmap p) sz) = complement `fmap` foldM addByte 0 [0..(sz - 1)]
	where addByte acc i = (p `peekElemOff` i) >>= \w -> return (acc + fromIntegral w)

-- | returns the padded size of a BAT
batGetSize :: Header -> Footer -> Int
batGetSize header footer = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
	where
		diskSize       = footerCurrentSize footer
		blockSize      = headerBlockSize header
		ents           = diskSize `divRoundUp` fromIntegral blockSize
		maxEntries     = headerMaxTableEntries header

batRead :: Bat -> Int -> IO Word32
batRead (Bat bptr _ _) n = peekBE ptr
	where ptr = bptr `plusPtr` (n*4)

batWrite :: Bat -> Int -> Word32 -> IO ()
batWrite (Bat bptr _ bmap) n v = pokeBE ptr v >> maybe (return ()) (batmapSet n) bmap
	where ptr = bptr `plusPtr` (n*4)

batMmap :: FilePath -> Header -> Footer -> Maybe BatmapHeader -> (Bat -> IO a) -> IO a
batMmap file header footer batmapHeader f =
	mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
		let batmap    = Batmap (Bitmap (castPtr (ptr `plusPtr` batmapOffset))) batmapSize in
		let batendPtr = ptr `plusPtr` batSize in
		f (Bat (castPtr ptr) batendPtr (maybe Nothing (const $ Just batmap) batmapHeader))
	where
		absoluteOffset   = fromIntegral (headerTableOffset header)
		offsetSize       = (absoluteOffset, fromIntegral (batSize + maybe 0 (const 512) batmapHeader + batmapSize))
		batmapOffset     = batSize + fromIntegral sectorLength
		batSize          = batGetSize header footer
		batmapSize       = maybe 0 (fromIntegral . (* sectorLength) . batmapHeaderSize) batmapHeader

batIterate :: Bat -> Int -> (Int -> Word32 -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0..(nb - 1)] (\i -> batRead bat i >>= \n -> f i n)

-- | update checksum in the batmap if the batmap exists
batUpdateChecksum :: Bat -> IO ()
batUpdateChecksum (Bat _ _        Nothing)       = return ()
batUpdateChecksum (Bat _ endptr   (Just batmap)) = do
	let batmapCheckSumPtr = endptr `plusPtr` (8+8+4+4)
	checkSum <- batmapChecksum batmap
	pokeBE batmapCheckSumPtr checkSum
