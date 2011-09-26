import Test.QuickCheck
import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import Data.Serialize
import Data.VHD.Types
import Data.VHD.Serialize

instance Arbitrary Version where
	arbitrary = Version <$> arbitrary <*> arbitrary

instance Arbitrary UniqueId where
	arbitrary = uniqueId . B.pack <$> replicateM 16 arbitrary

instance Arbitrary Cookie where
	arbitrary = cookie . B.pack <$> replicateM 8 arbitrary

instance Arbitrary CreatorApplication where
	arbitrary = creatorApplication . B.pack <$> replicateM 4 arbitrary

instance Arbitrary ParentLocatorEntry where
	arbitrary = parentLocatorEntry . B.pack <$> replicateM 24 arbitrary

instance Arbitrary ParentLocatorEntries where
	arbitrary = parentLocatorEntries <$> replicateM 8 arbitrary

instance Arbitrary ParentUnicodeName where
	arbitrary = parentUnicodeName . B.pack <$> replicateM 512 arbitrary

instance Arbitrary DiskGeometry where
	arbitrary = DiskGeometry <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DiskType where
	arbitrary = elements [ DiskTypeFixed, DiskTypeDynamic, DiskTypeDifferencing ]

instance Arbitrary CreatorHostOs where
	arbitrary = elements [ CreatorHostOsUnknown, CreatorHostOsWindows, CreatorHostOsMacintosh ]

instance Arbitrary Header where
	arbitrary = Header
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary

instance Arbitrary Footer where
	arbitrary = Footer
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary

prop_header_marshalling_id :: Header -> Bool
prop_header_marshalling_id h = decode (encode h) == Right h

prop_footer_marshalling_id :: Footer -> Bool
prop_footer_marshalling_id f = decode (encode f) == Right f

marshallingTests =
	[ testProperty "header identity" prop_header_marshalling_id
	, testProperty "footer identity" prop_footer_marshalling_id
	]

main = defaultMain
	[ testGroup "marshalling" marshallingTests
	]
