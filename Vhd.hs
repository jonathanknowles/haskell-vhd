import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.IORef
import Data.Vhd
import Data.Vhd.Bat
import qualified Data.Vhd.Block as Block
import Data.Vhd.Checksum
import Data.Vhd.Node
import Data.Vhd.Types
import System.Environment (getArgs)
import System.IO
import Text.Printf

cmdConvert [fileRaw, fileVhd, size] = do
	create fileVhd $ defaultCreateParameters
		{ size = read size * 1024 * 1024
		, useBatmap = False
		}
	withVhdNode fileVhd $ \node -> do
		withFile fileRaw ReadMode $ \handle -> do
			loop node handle 0
	where
		loop node handle offset = do
			srcblock <- B.hGet handle (fromIntegral blockSize)
			if B.null srcblock
				then return ()
				else do
					unless (isBlockZero srcblock) $ do
						let blockNumber = offset `div` fromIntegral blockSize
						appendEmptyBlock node blockNumber
						sectorOff <- unsafeSectorOffsetOfBlockNumber (nodeBat node) blockNumber
						Block.withBlock (nodeFilePath node) (headerBlockSize $ nodeHeader node) sectorOff $ \block ->
							Block.writeDataRange block 0 srcblock
						putStrLn ("block " ++ show (offset `div` fromIntegral blockSize) ++ " written")
					-- do something with block
					loop node handle (offset + fromIntegral (B.length srcblock))
		isBlockZero = B.all ((==) 0)
		blockSize = 2 * 1024 * 1024
cmdConvert _ = error "usage: convert <raw file> <vhd file> <size MiB>"

cmdCreate [name, size] = create name $ defaultCreateParameters { size = (read size) * 1024 * 1024, useBatmap = True }
cmdCreate _            = error "usage: create <name> <size MiB>"

cmdExtract [fileVhd, fileRaw] = withVhd fileVhd $ readData >=> BL.writeFile fileRaw
cmdExtract _                  = error "usage: extract <vhd file> <raw file>"

cmdPropGet [file, key] = withVhdNode file $ \node -> do
	case map toLower key of
		"max-table-entries"   -> putStrLn $ show $ headerMaxTableEntries   $ nodeHeader node
		"blocksize"           -> putStrLn $ show $ headerBlockSize         $ nodeHeader node
		"disk-type"           -> putStrLn $ show $ footerDiskType          $ nodeFooter node
		"current-size"        -> putStrLn $ show $ footerCurrentSize       $ nodeFooter node
		"uuid"                -> putStrLn $ show $ footerUniqueId          $ nodeFooter node
		"parent-uuid"         -> putStrLn $ show $ headerParentUniqueId    $ nodeHeader node
		"parent-timestamp"    -> putStrLn $ show $ headerParentTimeStamp   $ nodeHeader node
		"parent-filepath"     -> putStrLn $ show $ headerParentUnicodeName $ nodeHeader node
		"timestamp"           -> putStrLn $ show $ footerTimeStamp         $ nodeFooter node
		_                     -> error "unknown key"
cmdPropGet _ = error "usage: prop-get <file> <key>"

cmdRead [file] = withVhdNode file $ \node -> do
	let hdr = nodeHeader node
	let ftr = nodeFooter node
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
	batIterate (nodeBat node) (fromIntegral $ headerMaxTableEntries hdr) $ \i n -> do
		unless (n == 0xffffffff) $ modifyIORef allocated ((+) 1) >> printf "BAT[%.5x] = %08x\n" i n
	nb <- readIORef allocated
	putStrLn ("blocks allocated  : " ++ show nb ++ "/" ++ show (headerMaxTableEntries hdr))
cmdRead _ = error "usage: read <file>"

showBlockSize i
	| i < 1024     = printf "%d bytes" i
	| i < (1024^2) = printf "%d KiB" (i `div` 1024)
	| i < (1024^3) = printf "%d MiB" (i `div` (1024^2))
	| otherwise    = printf "%d GiB" (i `div` (1024^3))

showChecksum checksum isValid =
	printf "%08x (%s)" checksum (if isValid then "valid" else "invalid")

main = do
	args <- getArgs
	case args of
		"convert" :xs -> cmdConvert xs
		"create"  :xs -> cmdCreate  xs
		"extract" :xs -> cmdExtract xs
		"prop-get":xs -> cmdPropGet xs
		"read"    :xs -> cmdRead    xs
