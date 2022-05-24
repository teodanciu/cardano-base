{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Binary.Annotated
  ( Annotated(..)
  , ByteSpan(..)
  , Decoded(..)
  , annotationBytes
  , annotatedDecoder
  , slice
  , fromCBORAnnotated
  , decodeFullAnnotatedBytes
  , reAnnotate
  , Annotator (..)
  , annotatorSlice
  , decodeAnnotator
  , withSlice
  , FullByteString (..)
  )
where

import Prelude 

import Codec.CBOR.Read (ByteOffset)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Bifunctor(Bifunctor, first, second)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Function (on)
import Data.Functor((<&>))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

import Cardano.Binary.Deserialize (decodeFullDecoder)
import Cardano.Binary.FromCBOR
  (Decoder, DecoderError, FromCBOR(..), decodeWithByteSpan)
import Cardano.Binary.ToCBOR
  (ToCBOR)
import Cardano.Binary.Serialize (serialize')



-- | Extract a substring of a given ByteString corresponding to the offsets.
slice :: BSL.ByteString -> ByteSpan -> BSL.ByteString
slice bytes (ByteSpan start end) =
  BSL.take (end - start) $ BSL.drop start bytes

-- | A pair of offsets delimiting the beginning and end of a substring of a ByteString
data ByteSpan = ByteSpan !ByteOffset !ByteOffset
  deriving (Generic, Show)

-- Used for debugging purposes only.
instance ToJSON ByteSpan where

data Annotated b a = Annotated { unAnnotated :: !b, annotation :: !a }
  deriving (Eq, Show, Functor, Generic)
  deriving anyclass (NFData, NoThunks)

instance Bifunctor Annotated where
  first f (Annotated b a) = Annotated (f b) a
  second = fmap

instance (Eq a, Ord b) => Ord (Annotated b a) where
  compare = compare `on` unAnnotated

instance ToJSON b => ToJSON (Annotated b a) where
  toJSON = toJSON . unAnnotated

instance FromJSON b => FromJSON (Annotated b ()) where
  parseJSON j = flip Annotated () <$> parseJSON j

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
annotatedDecoder :: Decoder s a -> Decoder s (Annotated a ByteSpan)
annotatedDecoder d = decodeWithByteSpan d
  <&> \(x, start, end) -> Annotated x (ByteSpan start end)

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
fromCBORAnnotated :: FromCBOR a => Decoder s (Annotated a ByteSpan)
fromCBORAnnotated = annotatedDecoder fromCBOR

annotationBytes :: Functor f => BSL.ByteString -> f ByteSpan -> f BS.ByteString
annotationBytes bytes = fmap (BSL.toStrict . slice bytes)

-- | Decodes a value from a ByteString, requiring that the full ByteString is consumed, and
-- replaces ByteSpan annotations with the corresponding substrings of the input string.
decodeFullAnnotatedBytes
  :: Functor f
  => Text
  -> (forall s . Decoder s (f ByteSpan))
  -> BSL.ByteString
  -> Either DecoderError (f BS.ByteString)
decodeFullAnnotatedBytes lbl decoder bytes =
  annotationBytes bytes <$> decodeFullDecoder lbl decoder bytes

-- | Reconstruct an annotation by re-serialising the payload to a ByteString.
reAnnotate :: ToCBOR a => Annotated a b -> Annotated a BS.ByteString
reAnnotate (Annotated x _) = Annotated x (serialize' x)

class Decoded t where
  type BaseType t :: Type
  recoverBytes :: t -> BS.ByteString

instance Decoded (Annotated b BS.ByteString) where
  type BaseType (Annotated b BS.ByteString) = b
  recoverBytes = annotation

-------------------------------------------------------------------------
-- Annotator
-------------------------------------------------------------------------

-- | This marks the entire bytestring used during decoding, rather than the
-- | piece we need to finish constructing our value.
newtype FullByteString = Full BSL.ByteString

-- | A value of type `Annotator a` is one that needs access to the entire
-- | bytestring used during decoding to finish construction.
newtype Annotator a = Annotator { runAnnotator :: FullByteString -> a }
  deriving newtype (Monad, Applicative, Functor)

-- | The argument is a decoder for a annotator that needs access to the bytes that
-- | were decoded. This function constructs and supplies the relevant piece.
annotatorSlice :: Decoder s (Annotator (BSL.ByteString -> a)) -> Decoder s (Annotator a)
annotatorSlice dec = do
  (k,bytes) <- withSlice dec
  pure $ k <*> bytes

-- | Pairs the decoder result with an annotator.
withSlice :: Decoder s a -> Decoder s (a, Annotator BSL.ByteString)
withSlice dec = do
  (r, start, end) <- decodeWithByteSpan dec
  return (r, Annotator $ sliceOffsets start end)
  where
  sliceOffsets :: ByteOffset -> ByteOffset -> FullByteString -> BSL.ByteString
  sliceOffsets start end (Full b) = (BSL.take (end - start) . BSL.drop start) b

-- | Supplies the bytestring argument to both the decoder and the produced annotator.
decodeAnnotator :: Text -> (forall s. Decoder s (Annotator a)) -> BSL.ByteString -> Either DecoderError a
decodeAnnotator label' decoder bytes =
  (\x -> runAnnotator x (Full bytes)) <$> decodeFullDecoder label' decoder bytes
