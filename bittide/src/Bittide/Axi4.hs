{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Bittide.Axi4 where
import Clash.Prelude
import Protocols.Wishbone
import Data.Maybe

data AxiStreamM2S axiWidth = AxiStreamM2S
  { axisData  :: "axisData" ::: BitVector axiWidth
  , axisValid :: "axisValid" ::: Bool
  , axisLast  :: "axisLast" ::: Bool
  , axisUser  :: "axisUser" ::: Bool
  } deriving (Generic, NFDataX)

data AxiStreamS2M = AxiStreamS2M Bool
  deriving (Generic, NFDataX)

data AxiByteType axiWidth addrWidth wbWidth
  = ByteEnables
  | Address (Index (DivRU addrWidth axiWidth))
  | WriteData (Index (DivRU wbWidth axiWidth))
  deriving (Generic, NFDataX, Eq)

data AxiToWbState axiWidth addrWidth wbWidth = AxiToWbState
  { axiByteType   :: AxiByteType axiWidth addrWidth wbWidth
  , wbByteEnables :: BitVector (wbWidth `DivRU` 8)
  , wbAddress     :: Vec (addrWidth `DivRU` axiWidth) (BitVector axiWidth)
  , wbWriteData   :: Vec (wbWidth `DivRU` axiWidth) (BitVector axiWidth)
  , wbValid       :: Bool
  } deriving Generic

deriving instance (KnownNat axiWidth, 1 <= axiWidth, KnownNat addrWidth, KnownNat wbWidth)
  => NFDataX (AxiToWbState axiWidth addrWidth wbWidth)

-- data WbToAxisState axiWidth wbWidth = WbToAxisState
--   { encoderCount :: Maybe (Index (DivRU wbWidth axiWidth))
--   , storedRead   :: BitVector wbWidth
--   , lastRead     :: Bool
--   } deriving (Generic, NFDataX)

type AxiWbAdapterState axiWidth addrWidth wbWidth =
  ( AxiToWbState axiWidth addrWidth wbWidth
  , Maybe (WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth))
  )

axisToWishbone ::
  forall dom axiWidth addrWidth wbWidth .
  (HiddenClockResetEnable dom, KnownNat addrWidth, KnownNat axiWidth, 1 <= axiWidth, KnownNat wbWidth) =>
  Signal dom (AxiStreamM2S axiWidth) ->
  Signal dom (WishboneS2M (BitVector wbWidth)) ->
  ( Signal dom AxiStreamS2M
  , Signal dom (WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth))
  )
axisToWishbone axisM2S wbS2M = (axisS2M, wbM2S)
 where
  (axisS2M, wbM2S) = mealyB go initState (axisM2S, wbS2M)
  initState =
    ( AxiToWbState ByteEnables 0
      (deepErrorX "axisToWishbone: Initial wishbone address undefined")
      (deepErrorX "axisToWishbone: Initial wishbone writeData undefined")
      False
    , Nothing)
  go ::
    AxiWbAdapterState axiWidth addrWidth wbWidth ->
    (AxiStreamM2S axiWidth, WishboneS2M (BitVector wbWidth)) ->
    ( AxiWbAdapterState axiWidth addrWidth wbWidth
    , (AxiStreamS2M, WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth)))

  go (axiToWbState0@AxiToWbState{..}, wbOp) (AxiStreamM2S{..}, WishboneS2M{..}) =
    ((axiToWbState2, wbOpNext), output)
   where
    nextAxiByteType = case (axisLast, axisValid, wbValid,  axiByteType) of
      (True , _    , _    , _          )      -> ByteEnables
      (_    , _    , True , ByteEnables)      -> ByteEnables
      (_    , False, _    , _          )      -> axiByteType

      (False, True , False, ByteEnables)      -> Address 0
      (False, True , _    , Address n  )
        | n == maxBound && wbByteEnables == 0 -> ByteEnables
        | n == maxBound                       -> WriteData 0
        | otherwise                           -> Address (succ n)
      (False, True , _    , WriteData n)
        | n == maxBound                       -> ByteEnables
        | otherwise                           -> WriteData (succ n)

    wbLastByte = case (axisLast, axisValid, axiByteType) of
      (True, True, WriteData n) -> n == maxBound
      (True, True, Address n)   -> n == maxBound
      _                         -> False

    (wbOpNext, nextWbValid)
      | isJust wbOp && not acknowledge = (wbOp, wbValid || wbLastByte)
      | wbValid && (isNothing wbOp || acknowledge) = (Just $ (emptyWishboneM2S @addrWidth @(BitVector  wbWidth))
        { busCycle = True
        , strobe = True
        , busSelect = wbByteEnables
        , writeData = resize $ pack wbWriteData
        , addr = resize $ pack wbAddress}
        , False)
      | otherwise = (Nothing, wbValid || wbLastByte)


    axiToWbState1 = case (axiByteType, axisValid) of
      (ByteEnables, True) -> axiToWbState0{wbByteEnables = resize axisData}
      (Address n  , True) -> axiToWbState0{wbAddress     = replace n axisData wbAddress}
      (WriteData n, True) -> axiToWbState0{wbWriteData   = replace n axisData wbWriteData}
      (_, False)          -> axiToWbState0

    axiToWbState2 = axiToWbState1{axiByteType = nextAxiByteType, wbValid = nextWbValid}

    output =
      ( AxiStreamS2M (not wbValid || axiByteType /= ByteEnables)
      , fromMaybe emptyWishboneM2S wbOp)
