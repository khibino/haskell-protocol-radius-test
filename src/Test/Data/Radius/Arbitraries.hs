{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Data.Radius.Arbitraries (
  genPacket,
  ) where

import Test.QuickCheck (Arbitrary (..), Gen, oneof, elements)

import Test.Data.Radius.ArbitrariesBase (genPacket)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.String (IsString, fromString)
import qualified Data.Set as Set

import Data.Radius.Scalar
  (AtText (..), AtString (..), AtInteger (..))
import Data.Radius.Attribute
  (NumberAbstract (..), Attribute' (..), untypeNumber, Attribute (..),
   TypedNumberSets (..), )


instance Arbitrary v => Arbitrary (NumberAbstract v) where
  arbitrary = oneof [ Standard <$> arbitrary, Vendors <$> arbitrary ]

genSizedList :: Arbitrary a => Int -> Gen [a]
genSizedList n =
  elements [0..n] >>= (`replicateM` arbitrary)

genSizedString :: IsString a => Int -> Gen a
genSizedString n = fromString <$> genSizedList n

instance Arbitrary v => Arbitrary (Attribute' v) where
  arbitrary =
    oneof
    [ Attribute' . Standard <$> arbitrary <*> genSizedString (255 - 1 - 1)
    , Attribute' . Vendors  <$> arbitrary <*> genSizedString (255 - 1 - 1 - 4 - 1 - 1)
    ]

genAtText :: Int -> Gen AtText
genAtText n = AtText <$> genSizedString (n  `quot` 4)

genAtString :: Int -> Gen AtString
genAtString n = AtString <$> genSizedString n


instance TypedNumberSets v => Arbitrary (Attribute v AtText) where
  arbitrary = do
    n  <-  elements $ Set.toList attributeNumbersText
    case untypeNumber n of
      Standard _  -> Attribute n <$> genAtText (255 - 1 - 1)
      Vendors _   -> Attribute n <$> genAtText (255 - 1 - 1 - 4 - 1 - 1)

instance TypedNumberSets v => Arbitrary (Attribute v AtString) where
  arbitrary = do
    n  <-  elements $ Set.toList attributeNumbersString
    case untypeNumber n of
      Standard _  -> Attribute n <$> genAtString (255 - 1 - 1)
      Vendors _   -> Attribute n <$> genAtString (255 - 1 - 1 - 4 - 1 - 1)

instance TypedNumberSets v => Arbitrary (Attribute v AtInteger) where
  arbitrary =
    Attribute
    <$> elements (Set.toList attributeNumbersInteger)
    <*> arbitrary

{-
-- attributeNumbersIpV4 is empty list
instance Arbitrary (Attribute AtIpV4) where
  arbitrary =
    Attribute
    <$> elements (Set.toList attributeNumbersIpV4)
    <*> arbitrary
 -}
