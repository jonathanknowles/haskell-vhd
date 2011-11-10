module Data.Vhd.Bat
	( Bat (..)
	, batGetSize
	, containsBlock
	, lookupBlock
	, unsafeLookupBlock
	, batWrite
	, batMmap
	, batIterate
	, batUpdateChecksum
	) where

import Control.Monad
import Data.Bits
import Data.Storable.Endian
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Data.Vhd.Bitmap
import Data.Vhd.Serialize
import Data.Vhd.Types
import Data.Vhd.Utils
import System.IO.MMap

data Bat      = Bat BatStart BatEnd (Maybe Batmap)
type BatStart = Ptr PhysicalSectorAddress
type BatEnd   = Ptr PhysicalSectorAddress
data Batmap   = Batmap Bitmap Int

sectorLength = 512

emptyEntry = 0xffffffff

batmapSet :: VirtualBlockAddress -> Batmap -> IO ()
batmapSet n (Batmap bitmap _) = bitmapSet bitmap (fromIntegral n)

batmapChecksum :: Batmap -> IO Checksum
batmapChecksum (Batmap (Bitmap p) sz) = complement `fmap` foldM addByte 0 [0 .. (sz - 1)]
	where addByte acc i = (p `peekElemOff` i) >>= \w -> return (acc + fromIntegral w)

-- | returns the padded size of a BAT
batGetSize :: Header -> Footer -> Int
batGetSize header footer = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
	where
		maxEntries = headerMaxTableEntries header

containsBlock :: Bat -> VirtualBlockAddress -> IO Bool
containsBlock = (fmap (/= emptyEntry) .) . unsafeLookupBlock

lookupBlock :: Bat -> VirtualBlockAddress -> IO (Maybe PhysicalSectorAddress)
lookupBlock b n =
	fmap
		(\x -> if x == emptyEntry then Nothing else Just x)
		(unsafeLookupBlock b n)

unsafeLookupBlock :: Bat -> VirtualBlockAddress -> IO PhysicalSectorAddress
unsafeLookupBlock (Bat bptr _ _) n = peekBE ptr
	where ptr = bptr `plusPtr` ((fromIntegral n) * 4)

batWrite :: Bat -> VirtualBlockAddress -> PhysicalSectorAddress -> IO ()
batWrite (Bat bptr _ bmap) n v = pokeBE ptr v >> maybe (return ()) (batmapSet n) bmap
	where ptr = bptr `plusPtr` ((fromIntegral n) * 4)

batMmap :: FilePath -> Header -> Footer -> Maybe BatmapHeader -> (Bat -> IO a) -> IO a
batMmap file header footer batmapHeader f =
	mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
		let batmap    = Batmap (Bitmap (castPtr (ptr `plusPtr` batmapOffset))) batmapSize in
		let batendPtr = ptr `plusPtr` batSize in
		f . Bat (castPtr ptr) batendPtr $ fmap (const batmap) batmapHeader
	where
		absoluteOffset = fromIntegral (headerTableOffset header)
		offsetSize     = (absoluteOffset, fromIntegral (batSize + maybe 0 (const 512) batmapHeader + batmapSize))
		batmapOffset   = batSize + fromIntegral sectorLength
		batSize        = batGetSize header footer
		batmapSize     = maybe 0 (fromIntegral . (* sectorLength) . batmapHeaderSize) batmapHeader

batIterate :: Bat -> VirtualBlockAddress -> (VirtualBlockAddress -> PhysicalSectorAddress -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0 .. (nb - 1)] (\i -> unsafeLookupBlock bat i >>= \n -> f i n)

-- | update checksum in the batmap if the batmap exists
batUpdateChecksum :: Bat -> IO ()
batUpdateChecksum (Bat _ _        Nothing)       = return ()
batUpdateChecksum (Bat _ endptr   (Just batmap)) = do
	let batmapChecksumPtr = endptr `plusPtr` (8+8+4+4)
	checksum <- batmapChecksum batmap
	pokeBE batmapChecksumPtr checksum
