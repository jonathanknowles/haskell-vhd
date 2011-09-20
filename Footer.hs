module Footer where

import qualified FooterTypes as F
import qualified SharedTypes as S

data Footer = Footer
	{ cookie             :: S.Cookie
	, isTemporaryDisk    :: F.IsTemporaryDisk
	, formatVersion      :: F.FormatVersion
	, dataOffset         :: F.DataOffset
	, timeStamp          :: S.TimeStamp
	, creatorApplication :: F.CreatorApplication
	, creatorVersion     :: F.CreatorVersion
	, creatorHostOs      :: F.CreatorHostOs
	, originalSize       :: F.OriginalSize
	, currentSize        :: F.CurrentSize
	, diskGeometry       :: F.DiskGeometry
	, diskType           :: F.DiskType
	, checkSum           :: S.CheckSum
	, uniqueId           :: S.UniqueId
	, isSavedState       :: F.IsSavedState
	} deriving Show
