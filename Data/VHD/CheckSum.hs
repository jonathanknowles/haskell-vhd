module Data.VHD.CheckSum
	( adjustFooterChecksum
	, adjustHeaderChecksum
	, verifyFooterChecksum
	, verifyHeaderChecksum
	) where

import Data.VHD.Types
import Data.VHD.Serialize

import Data.Bits
import Data.Word
import Data.Serialize

import qualified Data.ByteString as B

plus :: CheckSum -> Word8 -> CheckSum
plus a b = a + fromIntegral b

getHeaderChecksum :: Header -> CheckSum
getHeaderChecksum header = complement $ B.foldl' plus 0 headerData
	where
		headerData = encode $ header { headerCheckSum = 0 }

getFooterChecksum :: Footer -> CheckSum
getFooterChecksum footer = complement $ B.foldl' plus 0 footerData
	where footerData = encode $ footer { footerCheckSum = 0 }

adjustFooterChecksum :: Footer -> Footer
adjustFooterChecksum f = f { footerCheckSum = checksum }
	where checksum = getFooterChecksum f

adjustHeaderChecksum :: Header -> Header
adjustHeaderChecksum h = h { headerCheckSum = checksum }
	where checksum = getHeaderChecksum h

verifyFooterChecksum :: Footer -> Bool
verifyFooterChecksum f = footerCheckSum f == checksum
	where checksum = getFooterChecksum f

verifyHeaderChecksum :: Header -> Bool
verifyHeaderChecksum h = headerCheckSum h == checksum
	where checksum = getHeaderChecksum h
