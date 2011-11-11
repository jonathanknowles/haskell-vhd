{-# LANGUAGE OverloadedStrings #-}

module Data.Vhd.Node
	( VhdNode (..)
	, containsBlock
	, lookupOrCreateBlock
	, withVhdNode
	) where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Char8 ()
import Data.IORef
import Data.Serialize (decode, encode)
import Data.Vhd.Block
import qualified Data.Vhd.Bat as Bat
import Data.Vhd.Types
import Data.Vhd.Utils
import Data.Vhd.Serialize
import System.IO

data VhdNode = VhdNode
	{ nodeBat      :: Bat.Bat
	, nodeHeader   :: Header
	, nodeFooter   :: Footer
	, nodeHandle   :: Handle
	, nodeFilePath :: FilePath
	, nodeModified :: IORef Bool
	}

withVhdNode :: FilePath -> (VhdNode -> IO a) -> IO a
withVhdNode filePath f = do
	withFile filePath ReadWriteMode $ \handle -> do
		footer <- either error id . decode <$> B.hGet handle 512
		header <- either error id . decode <$> B.hGet handle 1024
		mBatmapHdr <- if footerCreatorVersion footer == Version 1 3
			then do
				-- skip the BAT, and try reading the batmap header
				hSeek handle RelativeSeek (fromIntegral $ Bat.batGetSize header footer)
				batmapHdr <- decode <$> B.hGet handle 512
				case batmapHdr of
					Left _     -> return Nothing
					Right bHdr ->
						if batmapHeaderCookie bHdr == cookie "tdbatmap"
							then return $ Just bHdr
							else return Nothing
			else return Nothing
		Bat.batMmap filePath header footer mBatmapHdr $ \bat -> do
			bmodified <- newIORef False
			a <- f $ VhdNode
				{ nodeBat      = bat
				, nodeHeader   = header
				, nodeFooter   = footer
				, nodeHandle   = handle
				, nodeFilePath = filePath
				, nodeModified = bmodified
				}
			modified <- readIORef bmodified
			when (modified) $ Bat.batUpdateChecksum bat
			return a

lookupOrCreateBlock :: VhdNode -> VirtualBlockAddress -> IO PhysicalSectorAddress
lookupOrCreateBlock node blockNumber = do
	unlessM (Bat.containsBlock (nodeBat node) blockNumber) $ appendEmptyBlock node blockNumber
	Bat.unsafeLookupBlock (nodeBat node) blockNumber

containsBlock :: VhdNode -> VirtualBlockAddress -> IO Bool
containsBlock node = Bat.containsBlock (nodeBat node)

-- | create empty block at the end
appendEmptyBlock :: VhdNode -> VirtualBlockAddress -> IO ()
appendEmptyBlock node n = do
	hSeek (nodeHandle node) SeekFromEnd 512
	x <- hTell (nodeHandle node)
	-- paranoid check
	let (sector, m) = x `divMod` 512
	unless (m == 0) $ error "wrong sector alignment"
	Bat.batWrite (nodeBat node) n (fromIntegral sector)
	modifyIORef (nodeModified node) (const True)
	B.hPut (nodeHandle node) (B.replicate fullSize 0)
	hAlign (nodeHandle node) (fromIntegral sectorLength)
	B.hPut (nodeHandle node) $ encode (nodeFooter node)
	where
		fullSize   = bitmapSize + fromIntegral blockSize
		bitmapSize = bitmapSizeOfBlockSize blockSize
		blockSize  = headerBlockSize $ nodeHeader node
