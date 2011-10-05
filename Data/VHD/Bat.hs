module Data.VHD.Bat
	( Bat(..)
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

import Control.Monad

import System.IO.MMap

newtype Bat = Bat (Ptr Word32)
	deriving (Show,Eq)

-- | returns the padded size of a BAT
batGetSize :: Header -> Footer -> Int
batGetSize header footer = fromIntegral ((maxEntries * 4) `roundUpToModulo` sectorLength)
	where
		diskSize       = footerCurrentSize footer
		blockSize      = headerBlockSize header
		ents           = diskSize `divRoundUp` fromIntegral blockSize
		maxEntries     = headerMaxTableEntries header
		sectorLength   = 512

batRead :: Bat -> Int -> IO Word32
batRead (Bat bptr) n = peekBE ptr
	where ptr = bptr `plusPtr` (n*4)

batWrite :: Bat -> Int -> Word32 -> IO ()
batWrite (Bat bptr) n v = pokeBE ptr v
	where ptr = bptr `plusPtr` (n*4)

batMmap :: FilePath -> Header -> Footer -> (Bat -> IO a) -> IO a
batMmap file header footer f =
	mmapWithFilePtr file ReadWrite (Just offsetSize) $ \(ptr, sz) -> f (Bat $ castPtr ptr)
	where
		absoluteOffset = fromIntegral (headerTableOffset header)
		offsetSize     = (absoluteOffset, fromIntegral batSize)
		batSize        = batGetSize header footer

batIterate :: Bat -> Int -> (Int -> Word32 -> IO ()) -> IO ()
batIterate bat nb f = forM_ [0..(nb-1)] (\i -> batRead bat i >>= \n -> f i n)
