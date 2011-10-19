module Data.VHD.Bat
	( Bat(..)
	, batGetSize
	, batRead
	, batWrite
	, batMmap
	, batIterate
	) where

import Data.Word
import Data.Storable.Endian
import Foreign.Ptr
import Foreign.Storable

import Data.VHD.Types
import Data.VHD.Utils
import Data.VHD.Bitmap

import Control.Monad

import System.IO.MMap

data Bat = Bat (Ptr Word32) (Maybe Bitmap)

sectorLength   = 512

-- | returns the padded size of a BAT
batGetSize :: Header -> Footer -> Int
batGetSize header footer = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
	where
		diskSize       = footerCurrentSize footer
		blockSize      = headerBlockSize header
		ents           = diskSize `divRoundUp` fromIntegral blockSize
		maxEntries     = headerMaxTableEntries header

batRead :: Bat -> Int -> IO Word32
batRead (Bat bptr _) n = peekBE ptr
	where ptr = bptr `plusPtr` (n*4)

batWrite :: Bat -> Int -> Word32 -> IO ()
batWrite (Bat bptr _) n v = pokeBE ptr v
	where ptr = bptr `plusPtr` (n*4)

batMmap :: FilePath -> Header -> Footer -> Maybe BatmapHeader -> (Bat -> IO a) -> IO a
batMmap file header footer batmapHeader f =
	mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) ->
		let batmapPtr = maybe Nothing (const (Just $ Bitmap $ castPtr (ptr `plusPtr` batmapOffset))) batmapHeader in
		f (Bat (castPtr ptr) batmapPtr)
	where
		absoluteOffset   = fromIntegral (headerTableOffset header)
		offsetSize       = (absoluteOffset, fromIntegral (batSize + maybe 0 (const 512) batmapHeader + batmapSize))
		batmapOffset     = batSize + 512
		batSize          = batGetSize header footer
		batmapSize       = maybe 0 (fromIntegral . (* sectorLength) . batmapHeaderSize) batmapHeader

batIterate :: Bat -> Int -> (Int -> Word32 -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0..(nb-1)] (\i -> batRead bat i >>= \n -> f i n)
