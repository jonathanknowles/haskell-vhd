module Header where

import qualified HeaderTypes as H
import qualified SharedTypes as S

data Header = Header
	{ cookie               :: S.Cookie
	, dataOffset           :: H.DataOffset
	, tableOffset          :: H.TableOffset
	, headerVersion        :: H.HeaderVersion
	, maxTableEntries      :: H.MaxTableEntries
	, blockSize            :: H.BlockSize
	, checkSum             :: S.CheckSum
	, parentUniqueId       :: S.UniqueId
	, parentTimeStamp      :: S.TimeStamp
	, parentUnicodeName    :: H.ParentUnicodeName
	, parentLocatorEntries :: H.ParentLocatorEntries
	}
