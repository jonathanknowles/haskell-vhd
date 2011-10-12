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
import Data.VHD.Bat
import Data.VHD.Block
import Data.VHD.Utils
import Data.VHD.CheckSum
import Data.Bits
import Data.Word
import Data.Time.Clock.POSIX

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

create :: FilePath -> BlockSize -> Size -> IO ()
create filePath bs virtualSize = do
	now <- fromIntegral . fromEnum <$> getPOSIXTime
	createWithTimeStamp (fromIntegral (now - y2k)) filePath bs virtualSize
	where
		y2k :: Word64
		y2k = 946684800 -- seconds from the unix epoch to the vhd epoch

createWithTimeStamp :: TimeStamp -> FilePath -> BlockSize -> Size -> IO ()
createWithTimeStamp timeStamp filePath bs virtualSize =
	withFile filePath WriteMode $ \handle -> do
		id <- randomUniqueId
		B.hPut handle $ encode (DynamicDiskInfo (footer id) header)
		hAlign handle (fromIntegral sectorLength)
		-- create a BAT with every entry initialized to 0xffffffff.
		B.hPut handle $ B.replicate (fromIntegral batSize) 0xff

		hAlign handle (fromIntegral sectorLength)
		B.hPut handle $ encode (footer id)
	where
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral bs)
		batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
		batPadSize      = batSize - maxTableEntries * 4
		footerSize      = 512
		headerSize      = 1024 -- actually 1020

		footer id = adjustFooterChecksum $ Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = footerSize
			, footerTimeStamp          = timeStamp
			, footerCreatorApplication = creatorApplication "tap\0"
			, footerCreatorVersion     = Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = DiskGeometry 1 1 1 -- c h s wrong
			, footerDiskType           = DiskTypeDynamic
			, footerCheckSum           = 0
			, footerUniqueId           = id
			, footerIsSavedState       = False
			}
		header = adjustHeaderChecksum $ Header
			{ headerCookie               = cookie "cxsparse"
			, headerDataOffset           = 0xffffffffffffffff
			, headerTableOffset          = footerSize + headerSize
			, headerVersion              = Version 1 0
			, headerMaxTableEntries      = maxTableEntries
			, headerBlockSize            = bs
			, headerCheckSum             = 0
			, headerParentUniqueId       = uniqueId $ B.replicate 16 0
			, headerParentTimeStamp      = 0
			, headerReserved1            = B.replicate 4 0
			, headerParentUnicodeName    = parentUnicodeName ""
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f
