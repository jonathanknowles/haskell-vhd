module HeaderParser where

import Control.Applicative
import Data.Attoparsec
import Data.Attoparsec.Binary
import Data.Bits
import qualified Data.ByteString as BS
import Prelude hiding (take)

import qualified Header as H
import qualified HeaderTypes as HT
import HeaderTypes
import SharedTypes

header :: Parser H.Header
header = H.Header
	<$> (cookie               <$> take 8                     )
	<*> (dataOffset           <$> anyWord64be                )
	<*> (tableOffset          <$> anyWord64be                )
	<*> (headerVersion        <$> anyWord16be <*> anyWord16be)
	<*> (maxTableEntries      <$> anyWord32be                )
	<*> (blockSize            <$> anyWord32be                )
	<*> (checkSum             <$> anyWord32be                )
	<*> (parentUniqueId       <$> take 16                    )
	<*> (parentTimeStamp      <$> anyWord32be                )
	<*> (parentUnicodeName    <$> take 512                   )
	<*> (parentLocatorEntries <$> take 192                   )
	<*
	paddingBytes

paddingBytes = take 256

-- TODO: Provide a proper implementation of this function.
parentLocatorEntries _ = ParentLocatorEntries p p p p p p p p where
	p = HT.parentLocatorEntry (BS.replicate 24 0)
