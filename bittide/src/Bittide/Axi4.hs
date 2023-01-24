{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# LANGUAGE RecordWildCards #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Bittide.Axi4 where
import Clash.Prelude
import Protocols.Wishbone
import Data.Maybe

data ReducedAxiStreamM2S axiWidth = ReducedAxiStreamM2S
  { axisData  :: "axisData" ::: BitVector axiWidth
  , axisValid :: "axisValid" ::: Bool
  , axisLast  :: "axisLast" ::: Bool
  , axisUser  :: "axisUser" ::: Bool
  } deriving (Generic, NFDataX)


-- TODO: Remove Reduced types and switch over to types in Protocols.Axi4.Stream
data ReducedAxiStreamS2M = ReducedAxiStreamS2M Bool
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
  Signal dom (ReducedAxiStreamM2S axiWidth) ->
  Signal dom (WishboneS2M (BitVector wbWidth)) ->
  ( Signal dom ReducedAxiStreamS2M
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
    (ReducedAxiStreamM2S axiWidth, WishboneS2M (BitVector wbWidth)) ->
    ( AxiWbAdapterState axiWidth addrWidth wbWidth
    , (ReducedAxiStreamS2M, WishboneM2S addrWidth (DivRU wbWidth 8) (BitVector wbWidth)))

  go (axiToWbState0@AxiToWbState{..}, wbOp) (ReducedAxiStreamM2S{..}, WishboneS2M{..}) =
    ((axiToWbState2, wbOpNext), output)
   where
    nextAxiByteType = case (axisLast, axisValid, wbValid, axiByteType) of
      (True , True    , _    , _          )      -> ByteEnables
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

    wbLastByte = case (axisValid, axiByteType, wbByteEnables == 0) of
      (True, WriteData n, _)  -> n == maxBound
      (True, Address n, True) -> n == maxBound
      _                       -> False

    wbTerminated = acknowledge || err || retry
    (wbOpNext, nextWbValid)
      | isJust wbOp && not wbTerminated = (wbOp, wbValid || wbLastByte)
      | wbValid && (isNothing wbOp || wbTerminated) = (Just $ (emptyWishboneM2S @addrWidth @(BitVector  wbWidth))
        { busCycle = True
        , strobe = True
        , busSelect = wbByteEnables
        , writeData = resize $ pack wbWriteData
        , writeEnable = or $ False :> unpack wbByteEnables
        , addr = resize $ pack wbAddress}
        , False)
      | otherwise = (Nothing, wbValid || wbLastByte)


    axiToWbState1 = case (axiByteType, axisValid) of
      (ByteEnables, True) -> axiToWbState0{wbByteEnables = resize axisData}
      (Address n  , True) -> axiToWbState0{wbAddress     = replace n axisData (reverse wbAddress)}
      (WriteData n, True) -> axiToWbState0{wbWriteData   = replace n axisData (reverse wbWriteData)}
      (_, False)          -> axiToWbState0

    axiToWbState2 = axiToWbState1{axiByteType = nextAxiByteType, wbValid = nextWbValid}

    output =
      ( ReducedAxiStreamS2M (not wbValid || axiByteType /= ByteEnables)
      , fromMaybe emptyWishboneM2S wbOp)

{-# NOINLINE axisToWishbone #-}
