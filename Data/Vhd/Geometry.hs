module Data.Vhd.Geometry where

import Data.Vhd.Types
import Data.Word

-- | Calculates disk geometry using the algorithm presented in
-- | the virtual hard disk image format specification (v 1.0).
diskGeometry :: Word64 -> DiskGeometry
diskGeometry totalSectors' =
	let (sectorsPerTrack, heads, cylindersTimesHeads) = calculate in
	let cylinders = cylindersTimesHeads `div` heads in
	DiskGeometry
		(fromIntegral cylinders)
		(fromIntegral heads)
		(fromIntegral sectorsPerTrack)
	where
		totalSectors = min totalSectors' (65535 * 16 * 255)
		calculate
			| totalSectors > 65536 * 16 * 63  = (255, 16, totalSectors `div` 255)
			| otherwise                       =
				let sectorsPerTrack = 17 in
				let cylindersTimesHeads = totalSectors `div` 17 in
				let heads = max 4 $ (cylindersTimesHeads + 1023) `div` 1024 in
				let (sectorsPerTrack', heads', cylindersTimesHeads') =
					if cylindersTimesHeads >= heads * 1024 || heads > 16
						then (31, 16, totalSectors `div` 31)
						else (sectorsPerTrack, heads, cylindersTimesHeads) in
				if cylindersTimesHeads' >= heads' * 1024
					then (63, 16, totalSectors `div` 63)
					else (sectorsPerTrack', heads', cylindersTimesHeads')
