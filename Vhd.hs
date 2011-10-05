import Data.VHD
import Data.VHD.Types
import Data.VHD.Block
import Data.VHD.Context
import Data.VHD.Bat
import Data.VHD.CheckSum
import System.Environment (getArgs)
import System.IO
import Text.Printf
import Control.Monad

import Data.IORef

import qualified Data.ByteString as B

showBlockSize i
	| i < 1024     = printf "%d bytes" i
	| i < (1024^2) = printf "%d kilobytes" (i`div`1024)
	| i < (1024^3) = printf "%d megabytes" (i`div`(1024^2))
	| otherwise    = printf "%d gigabytes" (i`div`(1024^3))

readVhd file = withVhdContext file $ \ctx -> do
	let hdr = ctxHeader ctx
	let ftr = ctxFooter ctx
	mapM_ (\(f,s) -> putStrLn (f ++ " : " ++ s))
		[ ("cookie           ", show $ headerCookie hdr)
		, ("version          ", show $ headerVersion hdr)
		, ("max-table-entries", show $ headerMaxTableEntries hdr)
		, ("block-size       ", showBlockSize $ headerBlockSize hdr)
		, ("header-checksum  ", printf "%08x (%s)" (headerCheckSum hdr)
		                                           (if verifyHeaderChecksum hdr then "valid" else "invalid"))
		]
	mapM_ (\(f,s) -> putStrLn (f ++ " : " ++ s))
		[ ("disk-geometry    ", show $ footerDiskGeometry ftr)
		, ("original-size    ", showBlockSize $ footerOriginalSize ftr)
		, ("current-size     ", showBlockSize $ footerOriginalSize ftr)
		, ("type             ", show $ footerDiskType ftr)
		, ("footer-checksum  ", printf "%08x (%s)" (footerCheckSum ftr)
		                                           (if verifyFooterChecksum ftr then "valid" else "invalid"))
		]
	allocated <- newIORef 0
	batIterate (ctxBatPtr ctx) (fromIntegral $ headerMaxTableEntries hdr) $ \i n -> do
		unless (n == 0xffffffff) $ modifyIORef allocated ((+) 1) >> printf "BAT[%.5x] = %08x\n" i n
	nb <- readIORef allocated
	putStrLn ("block allocated   : " ++ show nb ++ "/" ++ show (headerMaxTableEntries hdr))

fromRaw fileRaw fileVhd size = do
	create fileVhd blockSize size
	withVhdContext fileVhd $ \ctx -> do
		withFile fileRaw ReadMode $ \handle -> do
			loop ctx handle 0
	where
		loop ctx handle offset = do
			srcblock <- B.hGet handle (fromIntegral blockSize)
			if B.null srcblock
				then return ()
				else do
					unless (isBlockZero srcblock) $ do
						let blockNb = offset `div` fromIntegral blockSize
						extendBlock ctx blockNb
						sectorOff <- batRead (ctxBatPtr ctx) blockNb
						withBlock (ctxFilePath ctx) (headerBlockSize $ ctxHeader ctx) sectorOff $ \block ->
							writeBlock block srcblock 0
						putStrLn ("block " ++ show (offset `div` fromIntegral blockSize) ++ " written")
					-- do something with block
					loop ctx handle (offset + fromIntegral (B.length srcblock))


		isBlockZero = B.all ((==) 0)
		blockSize = 2 * 1024 * 1024

main = do
	args <- getArgs
	case args of
		["create", file] -> create file (2 * 1024 * 1024) (1 * 1024 * 1024 * 1024)
		["read", file]   -> readVhd file
		["convert", file, vhdfile, size] -> fromRaw file vhdfile (read size*1024*1024*1024)
