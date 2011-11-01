{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Data.Vhd
	( create
	, CreateParameters (..)
	, defaultCreateParameters
	, getInfo
	-- * block related operations
	, Block
	, readDataRange
	, writeDataRange
	, withBlock
	-- * node related operations
	, VhdNode (..)
	, withVhdNode
	, appendEmptyBlock
	-- * exported Types
	, module Data.Vhd.Types
	) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.BitSet
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString.Char8 ()
import Data.Serialize
import Data.Vhd.Serialize
import Data.Vhd.Types
import Data.Vhd.Bat
import Data.Vhd.Block
import Data.Vhd.Node
import Data.Vhd.Utils
import Data.Vhd.Geometry
import Data.Vhd.Checksum
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Time.Clock.POSIX
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (subtract)

import System.IO

data Vhd = Vhd
	{ vhdBlockCount :: Word32
	, vhdBlockSize  :: BlockSize
	, vhdNodes      :: [VhdNode]
	}

withVhd :: FilePath -> (Vhd -> IO a) -> IO a
withVhd = withVhdInner [] where
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
	blockCount node = headerMaxTableEntries $ nodeHeader node
	blockSize  node = headerBlockSize       $ nodeHeader node
	diskType   node = footerDiskType        $ nodeFooter node
	parentPath node = p where
		ParentUnicodeName p = headerParentUnicodeName $ nodeHeader node

data CreateParameters = CreateParameters
	{ blockSize :: BlockSize
	, size      :: Size
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

-- | Reads raw data from a VHD chain.
read :: Vhd -> Word64 -> Int -> IO B.ByteString
read context byteOffset length = undefined

-- | Writes raw data to a VHD chain.
write :: Vhd -> Word64 -> B.ByteString -> IO ()
write context byteOffset rawData = undefined

copySubString :: CString -> CString -> CSize -> Int -> IO ()
copySubString source target length offset =
	B.memcpy (target `plusPtr` offset) (source `plusPtr` offset) length

-- | Reads a complete block from a VHD chain.
readBlock :: Vhd -> Int -> IO B.ByteString
readBlock vhd blockNumber =
		-- To do: modify this function so that it can read a sub-block.
		-- To do: reduce the use of intermediate data structures.
		fmap (updateResultWithNodeOffsets sectorsToRead) nodeOffsets >>
		return result
	where
		result = B.replicate (fromIntegral blockSize) 0
		blockSize = vhdBlockSize vhd
		sectorsToRead = fromRange 0 $ fromIntegral $ blockSize `div` sectorLength

		nodeOffsets :: IO [(VhdNode, Word32)]
		nodeOffsets = fmap catMaybes $ mapM maybeNodeOffset $ vhdNodes vhd

		maybeNodeOffset :: VhdNode -> IO (Maybe (VhdNode, Word32))
		maybeNodeOffset node =
			(fmap . fmap) (node, ) $ batReadMaybe (nodeBat node) blockNumber

		updateResultWithNodeOffsets :: BitSet -> [(VhdNode, Word32)] -> IO ()
		updateResultWithNodeOffsets _ [] = return ()
		updateResult sectorsMissing (nodeOffset : tail) =
			if Data.BitSet.isEmpty sectorsMissing then return () else do
			sectorsStillMissing <- updateResultWithNodeOffset sectorsMissing nodeOffset
			updateResult sectorsStillMissing tail

		updateResultWithNodeOffset :: BitSet -> (VhdNode, Word32) -> IO BitSet
		updateResultWithNodeOffset sectorsMissing (node, offset) =
			withBlock (nodeFilePath node) blockSize offset $ \block -> do
				deltaBitmap <- readBitmap block
				delta <- readData block
				let deltaSectorsPresent = fromByteString deltaBitmap
				let sectorsToCopy = sectorsMissing `intersect` deltaSectorsPresent
				let sectorsStillMissing = sectorsMissing `subtract` deltaSectorsPresent
				updateResultWithDelta delta sectorsToCopy
				return sectorsStillMissing

		updateResultWithDelta :: B.ByteString -> BitSet -> IO ()
		updateResultWithDelta delta sectorsToCopy =
			B.unsafeUseAsCString result  $ \resultPtr  ->
				B.unsafeUseAsCString delta $ \deltaPtr ->
					copySectors resultPtr deltaPtr
			where
				copySectors source target = forM_
					(map offsetOfSector $ toList sectorsToCopy)
					(copySubString source target $ fromIntegral sectorLength)
				offsetOfSector = (*) $ fromIntegral sectorLength
