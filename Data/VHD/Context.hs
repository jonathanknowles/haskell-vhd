module Data.VHD.Context
	( Context(..)
	, withVhdContext
	, extendBlock
	) where

import Data.VHD.Block
import Data.VHD.Bat
import Data.VHD.Types
import Data.VHD.Utils
import Data.VHD.Serialize

import Data.Serialize (decode, encode)

import qualified Data.ByteString as B

import System.IO

import Control.Applicative ((<$>))
import Control.Monad

data Context = Context
	{ ctxBatPtr    :: Bat
	, ctxBatmapPtr :: Bitmap
	, ctxHeader    :: Header
	, ctxFooter    :: Footer
	, ctxHandle    :: Handle
	, ctxFilePath  :: FilePath
	}

withVhdContext file f = do
	withFile file ReadWriteMode $ \handle -> do
		footer <- either error id . decode <$> B.hGet handle 512
		header <- either error id . decode <$> B.hGet handle 1024
		batMmap file header footer $ \bat -> do
		--batmapMmap file ddinfo $ \batmap -> do
			f $ Context
				{ ctxBatPtr    = bat
				, ctxBatmapPtr = undefined
				, ctxHeader    = header
				, ctxFooter    = footer
				, ctxHandle    = handle
				, ctxFilePath  = file
				}

-- | create empty block at the end
extendBlock ctx n = do
	hSeek (ctxHandle ctx) SeekFromEnd 512
	x <- hTell (ctxHandle ctx)
	-- paranoid check
	let (sector, m) = x `divMod` 512
	unless (m == 0) $ error "wrong sector alignment"
	batWrite (ctxBatPtr ctx) n (fromIntegral sector)

	B.hPut (ctxHandle ctx) (B.replicate fullSize 0)

	hAlign (ctxHandle ctx) (fromIntegral sectorLength)
	B.hPut (ctxHandle ctx) $ encode (ctxFooter ctx)

	where
		fullSize   = bitmapSize + fromIntegral blockSize
		bitmapSize = bitmapSizeOfBlock blockSize
		blockSize  = headerBlockSize $ ctxHeader ctx
