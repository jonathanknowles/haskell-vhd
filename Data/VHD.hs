{-# LANGUAGE OverloadedStrings #-}
module Data.VHD where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.Serialize
import Data.VHD.Serialize
import Data.VHD.Types
import Data.VHD.Utils
import Data.Bits
import Data.Word
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

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

getBat maxEntries = Bat <$> V.replicateM maxEntries getWord32be
putBat (Bat ents) = V.mapM_ putWord32be ents

create :: FilePath -> BlockSize -> Size -> IO ()
create filePath bs virtualSize = do
	BL.writeFile filePath $ runPutLazy $ do
		put (DynamicDiskInfo footer header)
		putBat bat
		put footer
	where
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral bs)
		batSize         = maxTableEntries * 4 `roundUpToModulo` sectorLength
		footerSize      = 512
		headerSize      = 1024

		bat = Bat (V.replicate (fromIntegral maxTableEntries) 0xffffffff)

		footer = Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = 0 -- wrong
			, footerTimeStamp          = 0 -- wrong
			, footerCreatorApplication = creatorApplication "vs  "
			, footerCreatorVersion     = Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = DiskGeometry 1 1 1 -- c h s wrong
			, footerDiskType           = DiskTypeDynamic
			, footerCheckSum           = 0 -- wrong
			, footerUniqueId           = randomUniqueId
			, footerIsSavedState       = False
			}
		header = Header
			{ headerCookie               = cookie "cxsparse"
			, headerDataOffset           = 0xffffffff
			, headerTableOffset          = footerSize + headerSize
			, headerVersion              = Version 1 0
			, headerMaxTableEntries      = maxTableEntries
			, headerBlockSize            = bs
			, headerCheckSum             = 0 -- wrong
			, headerParentUniqueId       = randomUniqueId
			, headerParentTimeStamp      = 0 -- wrong
			, headerParentUnicodeName    = parentUnicodeName $ B.replicate 512 0
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}
		randomUniqueId = uniqueId $ B.replicate 16 0 -- wrong

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f

readBat :: Handle -> DynamicDiskInfo -> IO Bat
readBat handle ddinfo = do
	hSeek handle AbsoluteSeek $ fromIntegral (headerTableOffset hdr)
	bs <- B.hGet handle (fromIntegral batSize)
	return $ Bat $ V.create (VM.new (fromIntegral maxEntries) >>= \v -> fill v 0 bs)
	where
		fill v i bs
			| i == maxEntries = return v
			| otherwise       = VM.write v (fromIntegral i) (be32 b1) >> fill v (i+1) b2
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


sectorLength = 512
