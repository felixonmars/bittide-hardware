-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Bittide.ClockControl.Callisto.Rust
  ( rustyCallisto
  ) where

import Clash.Prelude

import Foreign.C.Types (CUInt)
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, IntPtr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe

import Bittide.ClockControl (DataCount)
import Bittide.ClockControl.Callisto.Types
import Bittide.ClockControl.StabilityChecker

import Bittide.Simulate.RustFFI
import Bittide.Simulate.RustFFI.Sizes

import Data.Constraint
import Data.Constraint.Nat.Extra (OneMore, oneMore)

newtype VecS (n :: Nat) a = VecS (Vec n a)

type instance SizeOf (VecS n a) =
  SizeOf (Ptr a) + SizeOf Int + SizeOf Int + n * SizeOf a

type instance Alignment (VecS n a) =
  Alignment a

instance
  ( SizeOf (Ptr a) ~ SizeOf Int
  , Alignment (Ptr a) ~ Alignment Int
  , Storable a
  , KnownNat (SizeOf a)
  , KnownNat (Alignment a)
  , KnownNat n
  , 1 <= n
  )
  => Storable (VecS n a) where
  sizeOf = const $ natToNum @(SizeOf (VecS n a))
  alignment = const $ natToNum @(Alignment (VecS n a))

  peek p = do
    let h = natToNum @(SizeOf IntPtr)
        s = natToNum @(SizeOf a)
    vs <- sequence $ map (peekByteOff p . (+ 3*h) . (* s) . fromEnum) indicesI
    return $ VecS $ vs

  poke p (VecS v) = do
    let h = natToNum @(SizeOf IntPtr)
        s = natToNum @(SizeOf a)
        d = p `plusPtr` (3 * h)
    pokeByteOff p 0 d
    pokeByteOff p h (natToNum @n :: Int)
    pokeByteOff p (2*h) (natToNum @n :: Int)
    mapM_ (\(x, i) -> pokeByteOff d (s * fromEnum i) x) $ zip v indicesI

type VecI n = Vec n (Index n)

type BitsOf a = 8 * SizeOf a

type Elems n =
  Div n (BitsOf Int) + OneMore (Mod n (BitsOf Int))

newtype DataCountS (n :: Nat) = DataCountS (Signed n)
  deriving newtype (Show, Eq, Ord, Bits, Num, Real, Integral, Enum)

type instance SizeOf (DataCountS n) =
  SizeOf Int * Elems n

type instance Alignment (DataCountS n) =
  SizeOf Int

instance (KnownNat n, 1 <= n) => Storable (DataCountS n) where
  sizeOf = const $ natToNum @(SizeOf (DataCountS n))
  alignment = const $ natToNum @(Alignment (DataCountS n))

  peek p = case oneMore @n @(BitsOf Int) of
    Dict ->
      let
        s = natToNum @(BitsOf Int)

        v :: VecI (Elems n)
        v = indicesI

        toEnum' :: Int -> BitVector (BitsOf Int)
        toEnum' = toEnum
      in
       DataCountS . resize . unpack . pack
         <$> (sequence $ map (fmap toEnum' . peekByteOff p . (* s) . fromEnum) v)

  poke p (DataCountS c) = case oneMore @n @(BitsOf Int) of
    Dict ->
      let
        s = natToNum @(BitsOf Int)

        v :: Vec (Elems n) Int
        v = fromEnum <$> unpack' (pack $ resize' c)

        resize' :: DataCount n -> DataCount (BitsOf (DataCountS n))
        resize' = resize

        unpack' ::
          BitVector (BitsOf (DataCountS n)) ->
          Vec (Elems n) (BitVector (BitsOf Int))
        unpack' = unpack
      in
        mapM_ (\(x, i) -> pokeByteOff p (s * fromEnum i) x) $ zip v indicesI

-- | Variant of 'Bittide.ClockControl.Callisto.callisto', which is
-- implemented in Rust and uses the Rust FFI for being simulated.
rustyCallisto ::
  forall m n dom.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  , n <= BitsOf CUInt
  , m <= BitsOf CUInt
  ) =>
  -- | Configuration parameters.
  ControlConfig m ->
  -- | Update trigger.
  Signal dom Bool ->
  -- | Link availability mask.
  Signal dom (BitVector n) ->
  -- | Stability indicators for each of the elastic buffers.
  Signal dom (Vec n StabilityIndication) ->
  -- | Data counts from elastic buffers.
  Signal dom (Vec n (DataCount m)) ->
  -- | Current state.
  Signal dom ControlSt ->
  -- | Updated state.
  Signal dom ControlSt
rustyCallisto ControlConfig{..} shouldUpdate m scs counts st =
  upd <$> shouldUpdate <*> st
    <*> (callisto <$> m <*> scs <*> counts <*> st)
 where
  upd b old new =
    if b
    then new
    else new { _b_k = _b_k old }

  callisto mask stabilityChecks dataCounts state =
    unsafePerformIO $ do
      pState      <- malloc
      pVSI        <- malloc
      pDataCounts <- malloc

      poke pVSI $ VecS stabilityChecks
      poke pState state
      poke pDataCounts $ VecS (DataCountS <$> dataCounts)

      callisto_rust
        (toEnum $ fromEnum reframingEnabled)
        (fromInteger $ toInteger waitTime)
        (fromInteger $ toInteger targetCount)
        (fromInteger $ toInteger mask)
        (castPtr pVSI)
        (castPtr pDataCounts)
        (castPtr pState)

      state' <- peek pState

      free pState
      free pVSI
      free pDataCounts

      return state'
