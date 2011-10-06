module Data.VHD.Geometry where

import Data.VHD.Types
import Data.Word

-- | Calculates disk geometry using the algorithm presented in
-- | the virtual hard disk image format specification (v 1.0).
diskGeometry :: Word64 -> DiskGeometry
diskGeometry totalSectors =
	let totalSectors = min totalSectors (65535 * 16 * 255) in
	let (sectorsPerTrack, heads, cylindersTimesHeads) =
		if totalSectors > 65536 * 16 * 63
			then (255, 16, totalSectors `div` 255)
			else
				let sectorsPerTrack = 17 in
				let cylindersTimesHeads = totalSectors `div` 17 in
				let heads = max 4 $ (cylindersTimesHeads + 1023) `div` 1024 in
				let (sectorsPerTrack, heads, cylindersTimesHeads) =
					if cylindersTimesHeads >= heads * 1024 || heads > 16
						then (31, 16, totalSectors `div` 31)
						else (sectorsPerTrack, heads, cylindersTimesHeads) in
				let (sectorsPerTrack, heads, cylindersTimesHeads) =
					if cylindersTimesHeads >= heads * 1024
						then (63, 16, totalSectors `div` 63)
						else (sectorsPerTrack, heads, cylindersTimesHeads) in
				(sectorsPerTrack, heads, cylindersTimesHeads) in
	let cylinders = cylindersTimesHeads `div` heads in
	DiskGeometry
		(fromIntegral cylinders)
		(fromIntegral heads)
		(fromIntegral sectorsPerTrack)
