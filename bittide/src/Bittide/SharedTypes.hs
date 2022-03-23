{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=8 #-}
module Bittide.SharedTypes where
import Clash.Prelude

import Data.Proxy
import Data.Type.Equality ((:~:)(Refl))
import Data.Constraint
import Data.Constraint.Nat.Extra

type AtLeastOne n = (KnownNat n, 1 <= n)
type ByteEnable bytes = BitVector bytes
type DataLink frameWidth = Maybe (BitVector frameWidth)
type LessThan a b = (KnownNat a, KnownNat b, a <= b)
type NatFitsInBits n bits = NatRequiredBits n <= bits
type NatRequiredBits n = CLog 2 (n + 1)
type Paddable a = (BitPack a, NFDataX a, 1 <= BitSize a)
type TypeRequiredRegisters t regSize = DivRU (BitSize t) regSize
type WriteAny maxIndex any = Maybe (Index maxIndex, any)
type WriteBits maxIndex bits = Maybe (Index maxIndex, BitVector bits)
type WriteByte maxIndex = Maybe (Index maxIndex, BitVector 8)
type WriteBytes maxIndex bytes = Maybe (Index maxIndex, BitVector (bytes * 8))

type Pad a bw  = (Regs a bw * bw) - BitSize a
type Regs a bw = DivRU (BitSize a) bw

-- | Vector of registors that stores data coming from a communication bus. It can be used
-- to store any arbitrary data type, the last register is padded with p bits. The number of
-- registers and the amount of padding depends on the bit size of the stored data.
newtype RegisterBank regSize content =
  RegisterBank (Vec (Regs content regSize) (BitVector regSize))

instance (KnownNat regSize, 1 <= regSize ) => ShowX (RegisterBank regSize content) where
  showX (RegisterBank v) = showX v
  showsPrecX i (RegisterBank v) = showsPrecX i v

instance (AtLeastOne regSize, Paddable content) => NFDataX (RegisterBank regSize content) where
  deepErrorX str = paddedToRegisters @regSize @content $ Padded (deepErrorX str)
  hasUndefined (RegisterBank v) = hasUndefined v
  rnfX = rnfX
  ensureSpine = id

-- | Data type that ensures a fits in a register bank of size (n * bw), by
-- padding it with p bits.
newtype Padded bw a = Padded {paddedToData :: a}

instance (AtLeastOne bw, Paddable a, NFDataX a) => NFDataX (Padded bw a) where
  deepErrorX str = Padded (errorX str)
  hasUndefined (Padded a) = hasUndefined a
  rnfX = rnfX
  ensureSpine = id

bvAsPadded :: forall bw a. (Paddable a, AtLeastOne bw) => BitVector bw -> Padded bw a
bvAsPadded bv =
  case timesDivRU @bw @(BitSize a) of
    Dict -> case sameNat (Proxy @(Pad a bw + BitSize a)) (Proxy @bw) of
      Just Refl -> case split @_ @(Pad a bw) @(BitSize a) bv of
        (_pad,rest) -> Padded (unpack rest)
      _ -> error "bvAsPadded: Negative padding"

registersToData :: (Paddable a, AtLeastOne regSize) => RegisterBank regSize a -> a
registersToData = paddedToData . registersToPadded

paddedToRegisters :: forall bw a . (BitPack a, AtLeastOne bw) => Padded bw a -> RegisterBank bw a
paddedToRegisters (Padded a) = case timesDivRU @bw @(BitSize a) of
  Dict -> case SNat @(Pad a bw) of
    (SNat :: SNat p) -> RegisterBank (unpack ((0b0 :: BitVector p) ++# pack a))

registersToPadded :: forall bw a . (Paddable a, AtLeastOne bw) => RegisterBank bw a -> Padded bw a
registersToPadded (RegisterBank vec) =
  case timesDivRU @bw @(BitSize a) of
    Dict -> case split @_ @(Pad a bw) @(BitSize a) (pack vec) of
      (_pad,rest) -> Padded (unpack rest)
