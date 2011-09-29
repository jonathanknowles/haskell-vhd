module Data.VHD where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Serialize
import Data.VHD.Serialize
import Data.VHD.Types
import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V

import System.IO

data DynamicDiskInfo = DynamicDiskInfo
	{ footer :: ! Footer
	, header :: ! Header
	} deriving (Show,Eq)

instance Serialize DynamicDiskInfo where
	get = DynamicDiskInfo
		<$> get
		<*> get
	put d = do
		put $ footer d
		put $ header d

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f

readBat :: Handle -> DynamicDiskInfo -> IO Bat
readBat handle ddinfo = do
	hSeek handle AbsoluteSeek $ fromIntegral (headerTableOffset hdr)
	bs <- B.hGet handle (fromIntegral batSize)
	return $ Bat $ V.create (V.new (fromIntegral maxEntries) >>= \v -> fill v 0 bs)
	where
		fill v i bs
			| i == maxEntries = return v
			| otherwise       = V.write v (fromIntegral i) (be32 b1) >> fill v (i+1) b2
				where (b1,b2) = B.splitAt 4 bs

		be32 :: B.ByteString -> Word32
		be32 b = fromIntegral (B.index b 0) `shiftL` 24
		       + fromIntegral (B.index b 1) `shiftL` 16
		       + fromIntegral (B.index b 2) `shiftL` 8
		       + fromIntegral (B.index b 3)

		hdr        = header ddinfo
		diskSize   = footerCurrentSize $ footer ddinfo
		blockSize  = headerBlockSize hdr
		ents       = diskSize `divRoundUp` fromIntegral blockSize
		maxEntries = headerMaxTableEntries hdr
		batSize    = maxEntries * 4 `roundUpToModulo` sectorLength

		roundUpToModulo n m
			| n `mod` m == 0 = n
			| otherwise      = n + m - (n `mod` m)
		divRoundUp a b = let (d,m) = a `divMod` b in d + if m > 0 then 1 else 0

sectorLength = 512
