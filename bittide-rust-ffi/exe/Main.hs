{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Clash.Prelude
  ( HiddenClockResetEnable, KnownNat, Nat, Generic, NFDataX
  , Signal, Vec(..), Unsigned, Signed, Index, BitVector, Bits
  , Div, Mod, type (<=), type (+), type (*)
  , natToNum, testBit, setBit, clearBit, zeroBits
  )

import qualified Clash.Prelude as V

import Data.Proxy (Proxy(..))
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import System.IO.Unsafe
import Foreign.Marshal.Alloc

import GHC.TypeNats (natVal)
import GHC.TypeLits.KnownNat

import Bittide.Simulate.RustFFI
import Bittide.Simulate.RustFFI.Sizes

data SpeedChange
  = SpeedUp
  | SlowDown
  | NoChange
  deriving (Eq, Show, Enum)

type instance SizeOf SpeedChange = SizeOf CUInt
type instance Alignment SpeedChange = Alignment CUInt

instance (SizeOf SpeedChange ~ 4, Alignment SpeedChange ~ 4)
  => Storable SpeedChange where
  sizeOf = const $ natToNum @(SizeOf SpeedChange)
  alignment = const $ natToNum @(Alignment SpeedChange)

  peek p = from <$> peek (castPtr p :: Ptr CUInt)
   where
    from = \case
      0 -> SpeedUp
      1 -> SlowDown
      2 -> NoChange
      _ -> error "out of range"

  poke p = poke (castPtr p :: Ptr CUInt) . to
   where
    to = \case
      SpeedUp  -> 0
      SlowDown -> 1
      NoChange -> 2

data ReframingState =
    Detect
  | Wait
      { targetCorrection :: !Float
      , curWaitTime :: !(Unsigned 32)
      }
  | Done
  deriving (Show, Ord, Eq)

type instance SizeOf ReframingState =
  SizeOf CUInt + SizeOf Float + SizeOf CUInt
type instance Alignment ReframingState =
  Alignment CUInt

instance (SizeOf ReframingState ~ 12, Alignment ReframingState ~ 4)
  => Storable ReframingState where
  sizeOf = const $ natToNum @(SizeOf ReframingState)
  alignment = const $ natToNum @(Alignment ReframingState)

  peek p = (peekByteOff p 0 :: IO CUInt) >>= \case
    0 -> return Detect
    1 -> return Done
    2 -> Wait <$> peekByteOff p 4
              <*> ((fromIntegral :: CUInt -> Unsigned 32) <$> peekByteOff p 8)
    _ -> error "out of range"

  poke p = \case
    Detect   -> pokeByteOff p 0 (0 :: CUInt)
    Done     -> pokeByteOff p 0 (1 :: CUInt)
    Wait{..} -> do
      pokeByteOff p 0 (2 :: CUInt)
      pokeByteOff p 4 targetCorrection
      pokeByteOff p 8 (fromIntegral curWaitTime :: CUInt)

data ControlSt =
  ControlSt
    { _z_k :: !(Signed 32)
    , _b_k :: !SpeedChange
    , _steadyStateTarget :: !Float
    , rfState :: !ReframingState
    }
  deriving (Show)

type instance SizeOf ControlSt =
  SizeOf CInt + SizeOf SpeedChange + SizeOf Float + SizeOf ReframingState
type instance Alignment ControlSt =
  Alignment CInt

instance (SizeOf ControlSt ~ 24, Alignment ControlSt ~ 4)
  => Storable ControlSt where
  sizeOf = const $ natToNum @(SizeOf ControlSt)
  alignment = const $ natToNum @(Alignment ControlSt)

  peek p =
    ControlSt
      <$> ((fromIntegral :: CInt -> Signed 32) <$> peekByteOff p 0)
      <*> peekByteOff p 4
      <*> peekByteOff p 8
      <*> peekByteOff p 12

  poke p ControlSt{..} = do
    pokeByteOff p 0 (fromIntegral _z_k :: CInt)
    pokeByteOff p 4 _b_k
    pokeByteOff p 8 _steadyStateTarget
    pokeByteOff p 12 rfState

data StabilityIndication =
  StabilityIndication
    { stable :: Bool
    , settled :: Bool
    }
  deriving (Generic, NFDataX, Show)

type instance SizeOf StabilityIndication = SizeOf Int
type instance Alignment StabilityIndication = Alignment Int

instance Storable StabilityIndication where
  sizeOf = const $ natToNum @(SizeOf StabilityIndication)
  alignment = const $ natToNum @(Alignment StabilityIndication)

  peek p = fromC <$> peekByteOff p 0
   where
    fromC :: Int -> StabilityIndication
    fromC c = StabilityIndication (testBit c 0) (testBit c 1)

  poke p = pokeByteOff p 0 . toC
   where
     toC :: StabilityIndication -> Int
     toC (StabilityIndication x y) =
       let xBit = if x then (`setBit` 0) else (`clearBit` 0)
           yBit = if y then (`setBit` 1) else (`clearBit` 1)
       in  xBit $ yBit zeroBits

newtype VecS (n :: Nat) a =
  VecS { svec :: Vec n a } deriving newtype (Show)

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
    vs <- sequence $ V.map (peekByteOff p . (+ 3*h) . (* s) . fromEnum) V.indicesI
    return $ VecS $ vs

  poke p (VecS v) = do
    let h = natToNum @(SizeOf IntPtr)
        s = natToNum @(SizeOf a)
        d = p `plusPtr` (3 * h)
    pokeByteOff p 0 d
    pokeByteOff p h (natToNum @n :: Int)
    pokeByteOff p (2*h) (natToNum @n :: Int)
    mapM_ (\(x, i) -> pokeByteOff d (s * fromEnum i) x) $ V.zip v V.indicesI

type family RequiresMore (n :: Nat) :: Nat where
  RequiresMore 0 = 0
  RequiresMore _ = 1

type VecI n = Vec n (Index n)

instance KnownNat n => KnownNat1 $(nameToSymbol ''RequiresMore) n where
  natSing1 = case natVal (Proxy @n) of
    0 -> SNatKn 0
    _ -> SNatKn 1
  {-# INLINE natSing1 #-}

newtype Mask (n :: Nat) =
  Mask { mask :: BitVector n }
  deriving newtype (Show, Eq, Ord, Bits, Num, Real, Integral, Enum)

type instance SizeOf (Mask n) =
  SizeOf Int * ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int)))
type instance Alignment (Mask n) = SizeOf Int

instance
  ( KnownNat n
  , 1 <= n
  -- TODO: haxiom this out
  , 1 <= Div n (8 * SizeOf Int) + RequiresMore (Mod n (8 * SizeOf Int))
  ) => Storable (Mask n) where
  sizeOf = const $ natToNum @(SizeOf (Mask n))
  alignment = const $ natToNum @(Alignment (Mask n))

  peek p = Mask . V.resize . V.pack
    <$> (sequence $ V.map (fmap toEnum' . peekByteOff p . (* s) . fromEnum) v)
   where
    s = 8 * natToNum @(SizeOf Int)

    v :: VecI (((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int))))
    v = V.indicesI

    toEnum' :: (Int -> BitVector (8 * SizeOf Int))
    toEnum' = toEnum

  poke p (Mask bv) =
    mapM_ (\(x, i) -> pokeByteOff p (s * fromEnum i) x) $ V.zip v V.indicesI
   where
    s = 8 * natToNum @(SizeOf Int)

    v :: Vec ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int))) Int
    v = fromEnum <$> unpack (resize bv)

    resize :: BitVector n -> BitVector (8 * SizeOf (Mask n))
    resize = V.resize

    unpack ::
      BitVector (8 * SizeOf (Mask n)) ->
        Vec ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int)))
            (BitVector (8 * SizeOf Int))
    unpack = V.unpack

type DataCount n = Signed n

newtype DataCountS (n :: Nat) =
  DataCountS { dataCountS :: Signed n }
  deriving newtype (Show, Eq, Ord, Bits, Num, Real, Integral, Enum)

type instance SizeOf (DataCountS n) =
  SizeOf Int * ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int)))
type instance Alignment (DataCountS n) = SizeOf Int

instance
  ( KnownNat n
  , 1 <= n
  -- TODO: haxiom this out
  , 1 <= Div n (8 * SizeOf Int) + RequiresMore (Mod n (8 * SizeOf Int))
  ) => Storable (DataCountS n) where
  sizeOf = const $ natToNum @(SizeOf (DataCountS n))
  alignment = const $ natToNum @(Alignment (DataCountS n))

  peek p = DataCountS . V.resize . V.unpack . V.pack
    <$> (sequence $ V.map (fmap toEnum' . peekByteOff p . (* s) . fromEnum) v)
   where
    s = 8 * natToNum @(SizeOf Int)

    v :: VecI (((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int))))
    v = V.indicesI

    toEnum' :: (Int -> BitVector (8 * SizeOf Int))
    toEnum' = toEnum

  poke p (DataCountS c) =
    mapM_ (\(x, i) -> pokeByteOff p (s * fromEnum i) x) $ V.zip v V.indicesI
   where
    s = 8 * natToNum @(SizeOf Int)

    v :: Vec ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int))) Int
    v = fromEnum <$> unpack (V.pack $ resize c)

    resize :: DataCount n -> DataCount (8 * SizeOf (DataCountS n))
    resize = V.resize

    unpack ::
      BitVector (8 * SizeOf (DataCountS n)) ->
        Vec ((Div n (8 * SizeOf Int)) + RequiresMore (Mod n (8 * SizeOf Int)))
            (BitVector (8 * SizeOf Int))
    unpack = V.unpack

data ControlConfig (m :: Nat) =
  ControlConfig
    { reframingEnabled :: Bool
    , waitTime :: Unsigned 32
    , targetCount :: DataCount m
    }

callisto ::
  forall m n dom.
  ( HiddenClockResetEnable dom
  , KnownNat n
  , KnownNat m
  , 1 <= n
  , 1 <= m
  , n + m <= 32
  -- TODO: haxiom this out
  , 1 <= Div m (8 * SizeOf Int) + RequiresMore (Mod m (8 * SizeOf Int))
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
callisto cfg shouldUpdate mask scs dataCounts state =
  upd <$> shouldUpdate <*> state <*> r
 where
  r = callistoRust cfg <$> mask <*> scs <*> dataCounts <*> state
  upd b old new = if b then new else new { _b_k = _b_k old }

callistoRust ::
  ( KnownNat n
  , KnownNat m
  , 8 * SizeOf CUInt ~ 32
  , n <= 8 * SizeOf CUInt
  , 1 <= n
  , m <= 8 * SizeOf CUInt
  , 1 <= m
  -- TODO: haxiom this out
  , 1 <= Div m (8 * SizeOf Int) + RequiresMore (Mod m (8 * SizeOf Int))
  ) =>
  ControlConfig m ->
  BitVector n ->
  Vec n (StabilityIndication) ->
  Vec n (DataCount m) ->
  ControlSt ->
  ControlSt
callistoRust ControlConfig{..} mask stabilityChecks dataCounts state =
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

main :: IO ()
main =
  print $ callistoRust
    ControlConfig
      { reframingEnabled = True
      , waitTime = 439
      , targetCount = 0
      }
    0b110
    (  (StabilityIndication True False)
    :> (StabilityIndication False True)
    :> (StabilityIndication True True)
    :> Nil
    )
    ((8 :: DataCount 17) :> -2 :> 5 :> Nil)
    ControlSt
      { _z_k = 14
      , _b_k = SpeedUp
      , _steadyStateTarget = 3.2
      , rfState = Wait
          { targetCorrection = 2.3
          , curWaitTime = 33
          }
      }
