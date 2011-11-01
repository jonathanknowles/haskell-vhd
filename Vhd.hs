import Data.VHD
import Data.VHD.Types
import Data.VHD.Block
import Data.VHD.Context
import Data.VHD.Bat
import Data.VHD.Checksum
import System.Environment (getArgs)
import System.IO
import Text.Printf
import Control.Monad
import Data.Char

import Data.IORef

import qualified Data.ByteString as B

showBlockSize i
	| i < 1024     = printf "%d bytes" i
	| i < (1024^2) = printf "%d KiB" (i `div` 1024)
	| i < (1024^3) = printf "%d MiB" (i `div` (1024^2))
	| otherwise    = printf "%d GiB" (i `div` (1024^3))

cmdCreate [name, size] = create name $ defaultCreateParameters { size = (read size) * 1024 * 1024, useBatmap = True }
cmdCreate _            = error "usage: create <name> <size MiB>"

showChecksum checksum isValid =
	printf "%08x (%s)" checksum (if isValid then "valid" else "invalid")

cmdRead [file] = withVhdContext file $ \ctx -> do
	let hdr = ctxHeader ctx
	let ftr = ctxFooter ctx
	mapM_ (\(f, s) -> putStrLn (f ++ " : " ++ s))
		[ ("cookie           ", show $ headerCookie hdr)
		, ("version          ", show $ headerVersion hdr)
		, ("max-table-entries", show $ headerMaxTableEntries hdr)
		, ("block-size       ", showBlockSize $ headerBlockSize hdr)
		, ("header-checksum  ", showChecksum (headerChecksum hdr) (verifyHeaderChecksum hdr))
		, ("parent-uuid      ", show $ headerParentUniqueId hdr)
		, ("parent-filepath  ", show $ headerParentUnicodeName hdr)
		, ("parent-timestamp ", show $ headerParentTimeStamp hdr)
		]
	mapM_ (\(f, s) -> putStrLn (f ++ " : " ++ s))
		[ ("disk-geometry    ", show $ footerDiskGeometry ftr)
		, ("original-size    ", showBlockSize $ footerOriginalSize ftr)
		, ("current-size     ", showBlockSize $ footerOriginalSize ftr)
		, ("type             ", show $ footerDiskType ftr)
		, ("footer-checksum  ", showChecksum (footerChecksum ftr) (verifyFooterChecksum ftr))
		, ("uuid             ", show $ footerUniqueId ftr)
		, ("timestamp        ", show $ footerTimeStamp ftr)
		]
	allocated <- newIORef 0
	batIterate (ctxBat ctx) (fromIntegral $ headerMaxTableEntries hdr) $ \i n -> do
		unless (n == 0xffffffff) $ modifyIORef allocated ((+) 1) >> printf "BAT[%.5x] = %08x\n" i n
	nb <- readIORef allocated
	putStrLn ("blocks allocated  : " ++ show nb ++ "/" ++ show (headerMaxTableEntries hdr))
cmdRead _ = error "usage: read <file>"

cmdPropGet [file, key] = withVhdContext file $ \ctx -> do
	case map toLower key of
		"max-table-entries"   -> putStrLn $ show $ headerMaxTableEntries   $ ctxHeader ctx
		"blocksize"           -> putStrLn $ show $ headerBlockSize         $ ctxHeader ctx
		"disk-type"           -> putStrLn $ show $ footerDiskType          $ ctxFooter ctx
		"current-size"        -> putStrLn $ show $ footerCurrentSize       $ ctxFooter ctx
		"uuid"                -> putStrLn $ show $ footerUniqueId          $ ctxFooter ctx
		"parent-uuid"         -> putStrLn $ show $ headerParentUniqueId    $ ctxHeader ctx
		"parent-timestamp"    -> putStrLn $ show $ headerParentTimeStamp   $ ctxHeader ctx
		"parent-filepath"     -> putStrLn $ show $ headerParentUnicodeName $ ctxHeader ctx
		"timestamp"           -> putStrLn $ show $ footerTimeStamp         $ ctxFooter ctx
		_                     -> error "unknown key"
cmdPropGet _ = error "usage: prop-get <file> <key>"

cmdConvert [fileRaw, fileVhd, size] = do
	create fileVhd $ defaultCreateParameters
		{ size = read size * 1024 * 1024
		, useBatmap = True
		}
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
						appendEmptyBlock ctx blockNb
						sectorOff <- batRead (ctxBat ctx) blockNb
						withBlock (ctxFilePath ctx) (headerBlockSize $ ctxHeader ctx) sectorOff $ \block ->
							writeBlock block srcblock 0
						putStrLn ("block " ++ show (offset `div` fromIntegral blockSize) ++ " written")
					-- do something with block
					loop ctx handle (offset + fromIntegral (B.length srcblock))


		isBlockZero = B.all ((==) 0)
		blockSize = 2 * 1024 * 1024
cmdConvert _ = error "usage: convert <raw file> <vhd file> <size MiB>"

main = do
	args <- getArgs
	case args of
		"create"  :xs -> cmdCreate  xs
		"read"    :xs -> cmdRead    xs
		"convert" :xs -> cmdConvert xs
		"prop-get":xs -> cmdPropGet xs
