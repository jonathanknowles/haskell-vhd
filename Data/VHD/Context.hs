{-# LANGUAGE OverloadedStrings #-}
module Data.VHD.Context
	( Context (..)
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
import Data.ByteString.Char8 ()
import Data.IORef

import System.IO

import Control.Applicative ((<$>))
import Control.Monad

data Context = Context
	{ ctxBatPtr    :: Bat
	, ctxHeader    :: Header
	, ctxFooter    :: Footer
	, ctxHandle    :: Handle
	, ctxFilePath  :: FilePath
	, ctxBModified :: IORef Bool
	}

withVhdContext :: FilePath -> (Context -> IO a) -> IO a
withVhdContext file f = do
	withFile file ReadWriteMode $ \handle -> do
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
		batMmap file header footer mBatmapHdr $ \bat -> do
			bmodified <- newIORef False
			a <- f $ Context
				{ ctxBatPtr    = bat
				, ctxHeader    = header
				, ctxFooter    = footer
				, ctxHandle    = handle
				, ctxFilePath  = file
				, ctxBModified = bmodified
				}
			modified <- readIORef bmodified
			when (modified) $ batUpdateChecksum bat
			return a

-- | create empty block at the end
extendBlock ctx n = do
	hSeek (ctxHandle ctx) SeekFromEnd 512
	x <- hTell (ctxHandle ctx)
	-- paranoid check
	let (sector, m) = x `divMod` 512
	unless (m == 0) $ error "wrong sector alignment"
	batWrite (ctxBatPtr ctx) n (fromIntegral sector)
	modifyIORef (ctxBModified ctx) (const True)

	B.hPut (ctxHandle ctx) (B.replicate fullSize 0)

	hAlign (ctxHandle ctx) (fromIntegral sectorLength)
	B.hPut (ctxHandle ctx) $ encode (ctxFooter ctx)

	where
		fullSize   = bitmapSize + fromIntegral blockSize
		bitmapSize = bitmapSizeOfBlock blockSize
		blockSize  = headerBlockSize $ ctxHeader ctx
