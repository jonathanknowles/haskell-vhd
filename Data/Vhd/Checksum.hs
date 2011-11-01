module Data.Vhd.Checksum
	( adjustFooterChecksum
	, adjustHeaderChecksum
	, verifyFooterChecksum
	, verifyHeaderChecksum
	) where

import Data.Vhd.Types
import Data.Vhd.Serialize

import Data.Bits
import Data.Word
import Data.Serialize

import qualified Data.ByteString as B

plus :: Checksum -> Word8 -> Checksum
plus a b = a + fromIntegral b

getHeaderChecksum :: Header -> Checksum
getHeaderChecksum header = complement $ B.foldl' plus 0 headerData
	where
		headerData = encode $ header { headerChecksum = 0 }

getFooterChecksum :: Footer -> Checksum
getFooterChecksum footer = complement $ B.foldl' plus 0 footerData
	where footerData = encode $ footer { footerChecksum = 0 }

adjustFooterChecksum :: Footer -> Footer
adjustFooterChecksum f = f { footerChecksum = checksum }
	where checksum = getFooterChecksum f

adjustHeaderChecksum :: Header -> Header
adjustHeaderChecksum h = h { headerChecksum = checksum }
	where checksum = getHeaderChecksum h

verifyFooterChecksum :: Footer -> Bool
verifyFooterChecksum f = footerChecksum f == checksum
	where checksum = getFooterChecksum f

verifyHeaderChecksum :: Header -> Bool
verifyHeaderChecksum h = headerChecksum h == checksum
	where checksum = getHeaderChecksum h
