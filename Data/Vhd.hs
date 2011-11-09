{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Vhd
	( create
	, CreateParameters (..)
	, defaultCreateParameters
	, getInfo
	, readData
	, readDataRange
	, writeDataRange
	, withVhd
	, module Data.Vhd.Types
	) where

import Control.Applicative
import Control.Monad
import Data.BitSet
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.Maybe
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Vhd.Bat
import Data.Vhd.Block hiding (readData, readDataRange, writeDataRange)
import qualified Data.Vhd.Block as VB
import Data.Vhd.Checksum
import Data.Vhd.Geometry
import Data.Vhd.Node
import Data.Vhd.Types
import Data.Vhd.Utils
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (subtract)
import System.IO

data Vhd = Vhd
	{ vhdBlockCount :: VirtualBlockCount
	, vhdBlockSize  :: BlockByteCount
	, vhdNodes      :: [VhdNode]
	}

vhdLength :: Vhd -> Word64
vhdLength vhd = fromIntegral (vhdBlockCount vhd) * fromIntegral (vhdBlockSize vhd)

withVhd :: FilePath -> (Vhd -> IO a) -> IO a
withVhd = withVhdInner [] where

	blockCount node = headerMaxTableEntries $ nodeHeader node
	blockSize  node = headerBlockSize       $ nodeHeader node
	diskType   node = footerDiskType        $ nodeFooter node

	withVhdInner accumulatedNodes filePath f =
		withVhdNode filePath $ \node ->
			if diskType node == DiskTypeDifferencing
				then withVhdInner (node : accumulatedNodes) (parentPath node) f
				else f $ Vhd
					-- TODO: require consistent block count and size across all nodes.
					{ vhdBlockCount = blockCount node
					, vhdBlockSize  = blockSize  node
					, vhdNodes      = reverse $ node : accumulatedNodes
					}
		where parentPath node = resolveColocatedFilePath filePath p
			where ParentUnicodeName p = headerParentUnicodeName $ nodeHeader node

data CreateParameters = CreateParameters
	{ blockSize :: BlockByteCount
	, size      :: VirtualByteCount
	, timeStamp :: Maybe TimeStamp
	, uuid      :: Maybe UniqueId
	, useBatmap :: Bool
	} deriving (Show, Eq)

defaultCreateParameters = CreateParameters
	{ blockSize = 2 * 1024 * 1024
	, size      = 0
	, timeStamp = Nothing
	, uuid      = Nothing
	, useBatmap = False
	}

-- | grab the header and footer from a vhd file.
getInfo :: FilePath -> IO (Either String (Header, Footer))
getInfo filePath = withFile filePath ReadMode $ \handle -> do
	footer <- decode <$> B.hGet handle 512
	header <- decode <$> B.hGet handle 1024
	case (footer, header) of
		(Left err, _)      -> return $ Left err
		(_, Left err)      -> return $ Left err
		(Right f, Right h) -> return $ Right (h, f)

-- | create an empty vhd with the specified parameters
create :: FilePath -> CreateParameters -> IO ()
create filePath createParams
	| size createParams == 0 = error "cannot create a 0-sized VHD"
	| otherwise              = do
		nowUnixEpoch <- fromIntegral . fromEnum <$> getPOSIXTime
		let nowVhdEpoch = fromIntegral (nowUnixEpoch - y2k)
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
		B.hPut handle $ encode footer
		B.hPut handle $ encode header
		hAlign handle (fromIntegral sectorLength)
		-- create a BAT with every entry initialized to 0xffffffff.
		B.hPut handle $ B.replicate (fromIntegral batSize) 0xff

		-- maybe create a batmap
		when (useBatmap createParams) $ do
			hAlign handle (fromIntegral sectorLength)
			headerPos <- hTell handle
			B.hPut handle $ encode $ BatmapHeader
				{ batmapHeaderCookie   = cookie "tdbatmap"
				, batmapHeaderOffset   = fromIntegral (headerPos + fromIntegral sectorLength)
				, batmapHeaderSize     = (maxTableEntries `div` 8) `divRoundUp` sectorLength
				, batmapHeaderVersion  = Version 1 2
				, batmapHeaderChecksum = 0xffffffff
				}
			hAlign handle (fromIntegral sectorLength)
			B.hPut handle $ B.replicate (fromIntegral (maxTableEntries `div` 8)) 0x0

		hAlign handle (fromIntegral sectorLength)
		B.hPut handle $ encode footer
	where
		virtualSize     = size createParams
		maxTableEntries = fromIntegral (virtualSize `divRoundUp` fromIntegral (blockSize createParams))
		batSize         = (maxTableEntries * 4) `roundUpToModulo` sectorLength
		batPadSize      = batSize - maxTableEntries * 4
		footerSize      = 512
		headerSize      = 1024

		footer = adjustFooterChecksum $ Footer
			{ footerCookie             = cookie "conectix"
			, footerIsTemporaryDisk    = False
			, footerFormatVersion      = Version 1 0
			, footerDataOffset         = footerSize
			, footerTimeStamp          = fromJust $ timeStamp createParams
			, footerCreatorApplication = creatorApplication "tap\0"
			, footerCreatorVersion     = if useBatmap createParams then Version 1 3 else Version 1 0
			, footerCreatorHostOs      = CreatorHostOsWindows
			, footerOriginalSize       = virtualSize
			, footerCurrentSize        = virtualSize
			, footerDiskGeometry       = diskGeometry (virtualSize `div` fromIntegral sectorLength)
			, footerDiskType           = DiskTypeDynamic
			, footerChecksum           = 0
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
			, headerChecksum             = 0
			, headerParentUniqueId       = uniqueId $ B.replicate 16 0
			, headerParentTimeStamp      = 0
			, headerReserved1            = B.replicate 4 0
			, headerParentUnicodeName    = parentUnicodeName ""
			, headerParentLocatorEntries = parentLocatorEntries $ replicate 8 (ParentLocatorEntry $ B.replicate 24 0)
			}

-- | Reads all raw data from the given VHD.
readData :: Vhd -> IO BL.ByteString
readData vhd = readDataRange vhd 0 (vhdLength vhd)

-- | Reads raw data from within the given byte range of the given VHD.
readDataRange :: Vhd -> Word64 -> Word64 -> IO BL.ByteString
readDataRange vhd offset length =
	if offset + length > vhdLength vhd
		then error "upper bound cannot be greater than VHD length."
		else fmap (trim . BL.fromChunks) (sequence blocks)
	where
		blocks     = map (readDataBlock vhd) [blockFirst .. blockLast]
		blockFirst = fromIntegral $ (offset             ) `div` blockSize
		blockLast  = fromIntegral $ (offset + length - 1) `div` blockSize
		blockSize  = fromIntegral $ vhdBlockSize vhd
		trim       =  BL.take toTake . BL.drop toDrop
			where
				toTake = fromIntegral $ length
				toDrop = fromIntegral $ offset `mod` blockSize

-- | Writes raw data to the given VHD.
writeDataRange :: Vhd -> Word64 -> B.ByteString -> IO ()
writeDataRange vhd byteOffset rawData = undefined

-- | Reads raw data from within the given block of the given VHD.
readDataBlock :: Vhd -> Int -> IO B.ByteString
readDataBlock vhd blockNumber =
	B.create
		(fromIntegral $ blockSize)
		(unsafeReadDataBlock vhd blockNumber blockSize)
	where
		blockSize = vhdBlockSize vhd

unsafeReadDataBlock :: Vhd -> Int -> Word32 -> Ptr Word8 -> IO ()
unsafeReadDataBlock vhd blockNumber blockSize resultPtr = buildResult where

	-- To do: modify this function so that it can read a sub-block.
	-- To do: reduce the use of intermediate data structures.

	buildResult :: IO ()
	buildResult = do
		B.memset resultPtr 0 (fromIntegral blockSize)
		copySectorsFromNodes sectorsToRead =<< nodeOffsets

	sectorsToRead = fromRange 0 $ fromIntegral $ blockSize `div` sectorLength

	nodeOffsets :: IO [(VhdNode, Word32)]
	nodeOffsets = fmap catMaybes $ mapM maybeNodeOffset $ vhdNodes vhd where
		maybeNodeOffset node = (fmap . fmap) (node, ) $
			sectorOffsetOfBlockNumber (nodeBat node) blockNumber

	copySectorsFromNodes :: BitSet -> [(VhdNode, Word32)] -> IO ()
	copySectorsFromNodes sectorsToCopy [] = return ()
	copySectorsFromNodes sectorsToCopy (nodeOffset : tail) =
		if Data.BitSet.isEmpty sectorsToCopy then return () else do
		sectorsMissing <- copySectorsFromNode sectorsToCopy nodeOffset
		copySectorsFromNodes sectorsMissing tail

	copySectorsFromNode :: BitSet -> (VhdNode, Word32) -> IO BitSet
	copySectorsFromNode sectorsRequested (node, sectorOffset) =
		withBlock (nodeFilePath node) blockSize sectorOffset $ \block -> do
			deltaBitmap <- VB.readBitmap block
			let sectorsPresent = fromByteString deltaBitmap
			let sectorsMissing = sectorsRequested `subtract` sectorsPresent
			let sectorsToCopy = sectorsRequested `intersect` sectorsPresent
			mapM_
				(\offset -> unsafeReadDataRange block offset
					(fromIntegral sectorLength) (resultPtr `plusPtr` offset))
				(map byteOffsetOfSector $ toList sectorsToCopy)
			return sectorsMissing

	byteOffsetOfSector = (*) $ fromIntegral sectorLength
