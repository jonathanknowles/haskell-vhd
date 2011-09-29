import Data.VHD
import Data.VHD.Types
import System.Environment (getArgs)
import System.IO
import Text.Printf

showBlockSize i
	| i < 1024     = printf "%d bytes" i
	| i < (1024^2) = printf "%d kilobytes" (i`div`1024)
	| i < (1024^3) = printf "%d megabytes" (i`div`(1024^2))
	| otherwise    = printf "%d gigabytes" (i`div`(1024^3))

readVhd file = do
	ddinfo <- either error id `fmap` readDynamicDiskInfoFromFile file
	bat    <- withFile file ReadMode $ \handle -> readBat handle ddinfo
	let hdr = header ddinfo
	let ftr = footer ddinfo
	mapM_ (\(f,s) -> putStrLn (f ++ " : " ++ s))
		[ ("cookie           ", show $ headerCookie hdr)
		, ("version          ", show $ headerVersion hdr)
		, ("max-table-entries", show $ headerMaxTableEntries hdr)
		, ("block-size       ", showBlockSize $ headerBlockSize hdr)
		, ("checksum         ", printf "%08x" $ headerCheckSum hdr)
		]
	mapM_ (\(f,s) -> putStrLn (f ++ " : " ++ s))
		[ ("disk-geometry    ", show $ footerDiskGeometry ftr)
		, ("original-size    ", showBlockSize $ footerOriginalSize ftr)
		, ("current-size     ", showBlockSize $ footerOriginalSize ftr)
		, ("type             ", show $ footerDiskType ftr)
		, ("checksum         ", printf "%08x" $ footerCheckSum ftr)
		]
	---putStrLn $ show $ bat

main = do
	args <- getArgs
	let file = args !! 0
	case args of
		["create", file] -> create file (2 * 1024 * 1024) (2 * 1024 * 1024 * 1024)
		["read", file]   -> readVhd file
