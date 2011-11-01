{-# LANGUAGE OverloadedStrings #-}
module Data.VHD.Context
	( VhdNode (..)
	, withVhdNode
	, appendEmptyBlock
	) where

import Data.VHD.Block
import Data.VHD.Bat
import Data.VHD.Types
import Data.VHD.Utils
import Data.VHD.Serialize

import Data.Serialize (decode, encode)

import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.IORef

import System.IO

import Control.Applicative ((<$>))
import Control.Monad

data VhdNode = VhdNode
	{ nodeBat      :: Bat
	, nodeHeader   :: Header
	, nodeFooter   :: Footer
	, nodeHandle   :: Handle
	, nodeFilePath :: FilePath
	, nodeModified :: IORef Bool
	, nodeParent   :: Maybe VhdNode
	}

withVhdNode :: FilePath -> (VhdNode -> IO a) -> IO a
withVhdNode filePath f = do
	withFile filePath ReadWriteMode $ \handle -> do
		footer <- either error id . decode <$> B.hGet handle 512
		header <- either error id . decode <$> B.hGet handle 1024
		mBatmapHdr <- if footerCreatorVersion footer == Version 1 3
			then do
				-- skip the BAT, and try reading the batmap header
				hSeek handle RelativeSeek (fromIntegral $ batGetSize header footer)
				batmapHdr <- decode <$> B.hGet handle 512
				case batmapHdr of
					Left _     -> return Nothing
					Right bHdr ->
						if batmapHeaderCookie bHdr == cookie "tdbatmap"
							then return $ Just bHdr
							else return Nothing
			else return Nothing
		let buildNode parent =
			batMmap filePath header footer mBatmapHdr $ \bat -> do
				bmodified <- newIORef False
				a <- f $ VhdNode
					{ nodeBat      = bat
					, nodeHeader   = header
					, nodeFooter   = footer
					, nodeHandle   = handle
					, nodeFilePath = filePath
					, nodeModified = bmodified
					, nodeParent   = parent
					}
				modified <- readIORef bmodified
				when (modified) $ batUpdateChecksum bat
				return a
		if footerDiskType footer == DiskTypeDifferencing
			then do
				let ParentUnicodeName parentFilePath = headerParentUnicodeName header
				withVhdNode parentFilePath $ \parent -> buildNode $ Just parent
			else
				buildNode Nothing

contextChain :: VhdNode -> [VhdNode]
contextChain context = context : maybe [] contextChain (nodeParent context)

filePathChain :: VhdNode -> [FilePath]
filePathChain = map nodeFilePath . contextChain

handleChain :: VhdNode -> [Handle]
handleChain = map nodeHandle . contextChain

-- | create empty block at the end
appendEmptyBlock :: VhdNode -> Int -> IO ()
appendEmptyBlock node n = do
	hSeek (nodeHandle node) SeekFromEnd 512
	x <- hTell (nodeHandle node)
	-- paranoid check
	let (sector, m) = x `divMod` 512
	unless (m == 0) $ error "wrong sector alignment"
	batWrite (nodeBat node) n (fromIntegral sector)
	modifyIORef (nodeModified node) (const True)

	B.hPut (nodeHandle node) (B.replicate fullSize 0)

	hAlign (nodeHandle node) (fromIntegral sectorLength)
	B.hPut (nodeHandle node) $ encode (nodeFooter node)

	where
		fullSize   = bitmapSize + fromIntegral blockSize
		bitmapSize = bitmapSizeOfBlockSize blockSize
		blockSize  = headerBlockSize $ nodeHeader node
