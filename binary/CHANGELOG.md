# Changelog for `cardano-binary`

## 1.6.0.0

* Removed `Cardano.Binary.Annotated` and `Cardano.Binary.Drop` modules. They have been
  replaced by equivalent in
  [`cardano-ledger-binary`](https://github.com/input-output-hk/cardano-ledger/blob/master/libs/cardano-ledger-binary)
* Removed `Cardano.Binary.Raw`. It has moved into:
  [`cardano-crypto-wrapper:Cardano.Crypto.Raw`](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/byron/crypto/src/Cardano/Crypto/Raw.hs)
* Generalized `cborError` and `toCborError` to `MonadFail`
* Add `ToCBOR` instance for `Tokens -> Tokens`
* Add `To/FromCBOR` instances for `Term` and `ToCBOR` for `Encoding`
* Add `To/FromCBOR` instances for 6-tuples and 8-tuples
* Remove `FromCBOR` instance for `Ratio` in favor of `Rational`.
* Add `To/FromCBOR` instances for `Double`.
* Rename `toCBORMaybe` -> `encodeMaybe` with deprecation.
* Rename `decCBORMaybe` -> `decodeMaybe` with deprecation.
* Add `encodeNullMaybe` and `decodeNullMaybe`.
* Add `ToFromCBOR` instances for `Seq` and `StrictSeq`
* Deprecate `serializeEncoding` and `serializeEncoding'` in favor of `serialize` and
  `serialize'` respectively, since `Encoding` now has the `ToCBOR` instance.
* Add `decodeFullDecoder'` that accepts strict `ByteString`.