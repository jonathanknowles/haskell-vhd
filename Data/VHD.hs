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
import Data.Maybe
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

data CreateParameters = CreateParameters
	{ blockSize :: BlockSize
	, size      :: Size
	, timeStamp :: Maybe TimeStamp
	, uuid      :: Maybe UniqueId
	} deriving (Show,Eq)

defaultCreateParameters = CreateParameters
	{ blockSize = 2 * 1024 * 1024
	, size      = 0
	, timeStamp = Nothing
	, uuid      = Nothing
	}

create :: FilePath -> CreateParameters -> IO ()
create filePath createParams
	| size createParams == 0 = error "cannot create a 0-sized VHD"
	| otherwise              = do
		nowUnixEpoch <- fromIntegral . fromEnum <$> getPOSIXTime
		let nowVhdEpoch   = fromIntegral (nowUnixEpoch - y2k)
		uniqueid <- randomUniqueId
		create' filePath $ createParams
			{ uuid      = Just $ maybe uniqueid id $ uuid createParams
			, timeStamp = Just $ maybe nowVhdEpoch id $ timeStamp createParams
			}
	where
		y2k :: Word64
		y2k = 946684800 -- seconds from the unix epoch to the vhd epoch

create' :: FilePath -> CreateParameters -> IO ()
create' filePath createParams =
	withFile filePath WriteMode $ \handle -> do
		B.hPut handle $ encode (DynamicDiskInfo footer header)
		hAlign handle (fromIntegral sectorLength)
		-- create a BAT with every entry initialized to 0xffffffff.
		B.hPut handle $ B.replicate (fromIntegral batSize) 0xff

		hAlign handle (fromIntegral sectorLength)
		B.hPut handle $ encode footer
	where
		virtualSize     = size createParams
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral (blockSize createParams))
		batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
		batPadSize      = batSize - maxTableEntries * 4
		footerSize      = 512
		headerSize      = 1024 -- actually 1020

		footer = adjustFooterChecksum $ Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = footerSize
			, footerTimeStamp          = fromJust $ timeStamp createParams
			, footerCreatorApplication = creatorApplication "tap\0"
			, footerCreatorVersion     = Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = DiskGeometry 1 1 1 -- c h s wrong
			, footerDiskType           = DiskTypeDynamic
			, footerCheckSum           = 0
			, footerUniqueId           = fromJust $ uuid createParams
			, footerIsSavedState       = False
			}
		header = adjustHeaderChecksum $ Header
			{ headerCookie               = cookie "cxsparse"
			, headerDataOffset           = 0xffffffffffffffff
			, headerTableOffset          = footerSize + headerSize
			, headerVersion              = Version 1 0
			, headerMaxTableEntries      = maxTableEntries
			, headerBlockSize            = blockSize createParams
			, headerCheckSum             = 0
			, headerParentUniqueId       = uniqueId $ B.replicate 16 0
			, headerParentTimeStamp      = 0
			, headerReserved1            = B.replicate 4 0
			, headerParentUnicodeName    = parentUnicodeName ""
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}

readDynamicDiskInfoFromFile :: FilePath -> IO (Either String DynamicDiskInfo)
readDynamicDiskInfoFromFile f = return . decodeLazy =<< BL.readFile f
