-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}

module Tests.Axi4 where

import Clash.Prelude

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Sized.Vector(unsafeFromList)
import Data.Maybe
import Data.String
import Hedgehog
import Protocols.Axi4.Stream
import Protocols.Wishbone
import Test.Tasty
import Test.Tasty.Hedgehog

import Bittide.Axi4
import Bittide.Extra.Maybe
import Bittide.SharedTypes
import Tests.Shared

import qualified Data.List as L
import qualified GHC.TypeNats as TN
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Constraint
import Data.Constraint.Nat.Extra

axi4Group :: TestTree
axi4Group = testGroup "Axi4Group"
  [ testPropertyNamed
    "Axi4Stream upscaling does not affect packet content" "axisFromByteStreamUnchangedPackets"
    axisFromByteStreamUnchangedPackets
  , testPropertyNamed
    "Axi4Stream downscaling does not affect packet content" "axisToByteStreamUnchangedPackets"
    axisToByteStreamUnchangedPackets
  , testPropertyNamed
    "Read Axi4 Stream packets via Wishbone" "wbAxisRxBufferReadStreams"
    wbAxisRxBufferReadStreams
  , testPropertyNamed
    "Write Axi4 Stream packets via Wishbone" "wbAxisTxBufferWriteStreams"
    wbAxisTxBufferWriteStreams
  , testPropertyNamed
    "Write and read back Axi4 Stream packets via Wishbone" "wbAxisBuffersBoth"
    wbAxisBuffersBoth
  ]

type Packet = [Unsigned 8]
type BasicAxiConfig nBytes = 'Axi4StreamConfig nBytes 0 0

genAxisM2S :: KnownAxi4StreamConfig conf => Gen userType -> Gen (Axi4StreamM2S conf userType)
genAxisM2S genUser = do
  _tdata <- genVec $ genUnsigned Range.constantBounded
  _tkeep <- genVec Gen.bool
  _tstrb <- genVec Gen.bool
  _tlast <- Gen.bool
  _tid   <- genUnsigned Range.constantBounded
  _tdest <- genUnsigned Range.constantBounded
  _tuser <- genUser

  pure $ Axi4StreamM2S{..}

-- | Generate a `axisToByteStream` component with variable input bus width and test
-- if a stream of multiple generated `Packet`s can be routed through it without being changed.
axisToByteStreamUnchangedPackets :: Property
axisToByteStreamUnchangedPackets = property $ do
  busWidth <- forAll @_ @Integer $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 1 8
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 64
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  repeatingReadyList <- forAll $ Gen.filter or $ Gen.list (Range.linear 1 8) Gen.bool
  case
    TN.someNatVal $ fromIntegral (busWidth - 1) of
    SomeNat (succSNat . snatProxy -> _ :: SNat busWidth) -> do
      let
        axiStream = Nothing : L.concatMap (packetToAxiStream (SNat @busWidth)) packets
        topEntity = bundle (axisM2SSmall, axisS2MSmall, axisM2SBig, axisS2MBig, simRunning)
         where
          -- Run the simulation until there are no more axi stream operations and
          -- the fifo is empty, run for at least 10 cycles.
          simRunning =
            isJust <$> axisM2SSmall
            .||. isJust <$> axisM2SBig
            .||. unsafeToHighPolarity (resetGenN d10)
          axisS2MSmall = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MBig, axisM2SSmall) =
            wcre (axisToByteStream @System @_ @(BasicAxiConfig 1)) axisM2SBig axisS2MSmall
          axisM2SBig = wcre $ axi4StreamSimDriver axiStream axisS2MBig
        maxSimDuration = 10 + (2 * sum packetLengths * L.length repeatingReadyList)
      let
        (axisM2SSmall, axisS2MSmall,axisM2SBig, axisS2MBig, _) =
          L.unzip5 $ L.takeWhile (\(_,_,_,_,r) -> r) $ sampleN maxSimDuration topEntity
        retrievedPackets = axis4ToPackets axisM2SSmall axisS2MSmall
      footnote . fromString $ "axisM2SSmall:" <> show axisM2SSmall
      footnote . fromString $ "axisS2MSmall:" <> show axisS2MSmall
      footnote . fromString $ "axisM2SBig:" <> show axisM2SBig
      footnote . fromString $ "axisS2MBig:" <> show axisS2MBig
      packets  === retrievedPackets


-- | Generate a `axisFromByteStream` component with variable output bus width and test
-- if a stream of multiple generated `Packet`s can be routed through it without being changed.
axisFromByteStreamUnchangedPackets :: Property
axisFromByteStreamUnchangedPackets = property $ do
  busWidth <- forAll @_ @Integer $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 1 8
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 64
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  repeatingReadyList <- forAll $ Gen.filter or $ Gen.list (Range.linear 1 8) Gen.bool
  case
    TN.someNatVal $ fromIntegral (busWidth - 1) of
    SomeNat (succSNat . snatProxy -> _ :: SNat busWidth) -> do
      let
        axiStream = Nothing : L.concatMap (packetToAxiStream d1) packets
        topEntity = bundle (axisM2SBig, axisS2MBig, simRunning)
         where
          -- Run the simulation until there are no more axi stream operations and
          -- the fifo is empty, run for at least 10 cycles.
          simRunning =
            isJust <$> axisM2SSmall
            .||. isJust <$> axisM2SBig
            .||. unsafeToHighPolarity (resetGenN d10)
          axisS2MBig = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MSmall, axisM2SBig) = wcre @System $
            axisFromByteStream @_ @_ @(BasicAxiConfig busWidth) axisM2SSmall axisS2MBig
          -- Axi stream master
          axisM2SSmall = wcre $ axi4StreamSimDriver axiStream axisS2MSmall
        maxSimDuration = 10 + (2*sum packetLengths * L.length repeatingReadyList)
        (axisM2S, axisS2M, _) = L.unzip3 $ L.takeWhile (\(_,_,r) -> r) $ sampleN maxSimDuration topEntity
        retrievedPackets = axis4ToPackets axisM2S axisS2M
      packets  === retrievedPackets

wbAxisBuffersBoth :: Property
wbAxisBuffersBoth = property $ do
  fifoDepth <- forAll $ Gen.enum 1 16
  busWidth <- forAll $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 1 8
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 (busWidth * fifoDepth)
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  case
    ( TN.someNatVal $ fromIntegral (fifoDepth - 1)
    , TN.someNatVal $ fromIntegral (busWidth - 1)) of
    ( SomeNat (succSNat . snatProxy -> _ :: SNat fifoDepth)
     ,SomeNat (succSNat . snatProxy -> _ :: SNat busWidth)) -> do
      let
        wbReadPacket pl = unpack 0 :
          (wbRead @32 @busWidth <$> fifoDepth : [0..(pl `div` busWidth)]) <>
          [wbWrite @32 @busWidth fifoDepth 0, wbWrite (fifoDepth + 1) maxBound]
        wbOpsA = L.concat $ L.zipWith (packetsToWb @32 @busWidth fifoDepth) packetLengths packets
        wbOpsB = L.concatMap wbReadPacket packetLengths
        topEntity = bundle (wbM2SA, wbS2MA, wbM2SB, wbS2MB, axisM2S, axisS2M, wbStatus, simRunning)
         where
          (wbS2MA, axisM2S) = wcre $
            wbAxisTxBuffer @System @32 @busWidth @fifoDepth @(BasicAxiConfig busWidth)
            SNat wbM2SA axisS2M

          wbM2SA = case cancelMulDiv @busWidth @8 of
            Dict -> wcre $ wishboneSimDriver wbOpsA wbS2MA

          wbM2SB = case cancelMulDiv @busWidth @8 of
            Dict -> withClockResetEnable clockGen resetGen (toEnable enableWishboneB)
             $ wishboneSimDriver wbOpsB wbS2MB
          enableWishboneB = uncurry (||) <$> wbStatus

          (wbS2MB, axisS2M, wbStatus) = wcre $
            wbAxisRxBuffer @System @32 @busWidth @fifoDepth @(BasicAxiConfig busWidth)
            SNat wbM2SB axisM2S (pure (False, False))

          simRunning = not <$> (wbAInactive .&&. wbBInactive .&&. isNothing <$> axisM2S .&&. wbStatus .==. pure (False, False))
          wbAInactive = (\WishboneM2S{..} -> not $ busCycle || strobe) <$> wbM2SA
          wbBInactive = (\WishboneM2S{..} -> not $ busCycle || strobe) <$> wbM2SB
      let
        (wbWriteM2S, wbWriteS2M, wbReadM2S, wbReadS2M, axisM2S, axisS2M, wStatus, _) = unzip8 . L.takeWhile (\(_,_,_,_,_,_,_,r) -> r)
         $ sampleN ((10 + sum packetLengths) * 4) topEntity
        wbWriteTransactions = wbToTransaction wbWriteM2S wbWriteS2M
        wbReadTransactions = wbToTransaction wbReadM2S wbReadS2M
      footnote . fromString $ "wbWriteOps:" <> show wbOpsA
      footnote . fromString $ "wbReadOps:" <> show wbOpsB
      footnote . fromString $ "wbWriteTransactions:" <> show wbWriteTransactions
      footnote . fromString $ "wbReadTransactions:" <> show wbReadTransactions
      footnote . fromString $ "axisM2S:" <> show axisM2S
      footnote . fromString $ "axisS2M:" <> show axisS2M
      footnote . fromString $ "wStatus:" <> show wStatus
      packets === packetFromTransactions wbReadTransactions

unzip8 :: [(a,b,c,d,e,f,g,h)] -> ([a],[b],[c],[d],[e],[f],[g],[h])
unzip8 =  L.foldr (\(a,b,c,d,e,f,g,h) ~(as,bs,cs,ds,es,fs,gs,hs) ->
  (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs,h:hs)) ([], [] ,[] ,[] ,[] ,[] ,[], [])

wbAxisTxBufferWriteStreams :: Property
wbAxisTxBufferWriteStreams = property $ do
  fifoDepth <- forAll $ Gen.enum 1 8
  busWidth <- forAll $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 1 8
  repeatingReadyList <- forAll $ Gen.filter or $ Gen.list (Range.linear 1 8) Gen.bool
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 (busWidth * fifoDepth)
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  case
    ( TN.someNatVal $ fromIntegral (fifoDepth - 1)
    , TN.someNatVal $ fromIntegral (busWidth - 1)) of
    ( SomeNat (succSNat . snatProxy -> _ :: SNat fifoDepth)
     ,SomeNat (succSNat . snatProxy -> _ :: SNat busWidth)) -> do
      let
        wbOps = L.concat $ L.zipWith (packetsToWb @32 @busWidth fifoDepth) packetLengths packets
        topEntity = bundle (axisM2S, axisS2M, simRunning)
         where
          (wbS2M, axisM2S) = wcre $
            wbAxisTxBuffer @System @32 @busWidth @fifoDepth @(BasicAxiConfig busWidth)
            SNat wbM2S axisS2M
          axisS2M = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          wbM2S = case cancelMulDiv @busWidth @8 of
            Dict -> wcre $ wishboneSimDriver wbOps wbS2M
          simRunning = not <$> (isNothing <$> axisM2S .&&. wbInactive .&&. streamingStarted)
          streamingStarted = wcre $ register False (streamingStarted .||. isJust <$> axisM2S)
          wbInactive = (\WishboneM2S{..} -> not $ busCycle || strobe) <$> wbM2S

        (axisM2SList, axisS2MList, _) = L.unzip3 . L.takeWhile (\(_,_,r) -> r)
          $ sampleN (10 + fifoDepth * nrOfPackets *  L.length repeatingReadyList + L.length wbOps) topEntity

      footnote . fromString $ "wbOps:" <> show wbOps
      footnote . fromString $ "axisM2SList:" <> show axisM2SList
      footnote . fromString $ "axisS2MList:" <> show axisS2MList
      L.concat packets === axis4ToPackets axisM2SList axisS2MList

wbAxisRxBufferReadStreams :: Property
wbAxisRxBufferReadStreams = property $ do
  fifoDepth <- forAll $ Gen.enum 1 8
  busWidth <- forAll $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 0 8
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 (busWidth * fifoDepth)

  -- Generate list of packets [[Unsigned 8]]
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  -- Introduce fifoDepth and busWidth on type level
  case (TN.someNatVal $ fromIntegral (fifoDepth - 1), TN.someNatVal $ fromIntegral (busWidth - 1)) of
    ( SomeNat (succSNat . snatProxy -> depthSNat@SNat :: SNat fifoDepth)
     ,SomeNat (succSNat . snatProxy -> busWidthSNat :: SNat busWidth)) -> do
      let
        axiStream = Nothing : L.concatMap (packetToAxiStream busWidthSNat) packets

        wbReadPacket pl = unpack 0 :
          (wbRead @32 @busWidth <$> fifoDepth : [0..(pl `div` busWidth)]) <>
          [wbWrite @32 @busWidth fifoDepth 0, wbWrite (fifoDepth + 1) maxBound]

        -- Make wishbone operations for all packets.
        wbOps = L.concatMap wbReadPacket packetLengths
        topEntity = bundle (wbM2S, wbS2M, axisM2S, axisS2M, wbStatus, simRunning)
         where
          -- Run the simulation until there are no more axi stream operations and
          -- the fifo is empty, run for at least 10 cycles.
          simRunning =
            (uncurry (||) <$> wbStatus)
            .||. (isJust <$> axisM2S)
            .||. unsafeToHighPolarity (resetGenN d10)

          (wbS2M, axisS2M, wbStatus) = wcre $
            wbAxisRxBuffer @System @32 @busWidth
            depthSNat wbM2S axisM2S (pure (False, False))

          -- Axi stream master
          axisM2S = wcre $ axi4StreamSimDriver axiStream axisS2M

          -- Wishbone master
          wbM2S = case cancelMulDiv @busWidth @8 of
            Dict -> withClockResetEnable @System clockGen resetGen (toEnable enableWishbone)
              $ wishboneSimDriver wbOps wbS2M

          enableWishbone = uncurry (||) <$> wbStatus
        -- Run until the simulation is done.
        simOut = L.takeWhile (\(_,_,_,_,_,simRunning) -> simRunning) $ sample topEntity
        (m, s, _, _, _,_) = L.unzip6 simOut
        -- Transform the wishbone signals into Transactions and into Packets
        wbResults = packetFromTransactions $ wbToTransaction m s
      footnote . fromString $ "(wbM2S, wbS2M, axisM2S, axisS2M, wbStatus, simRunning): " <> show simOut
      footnote . fromString $ "wbOps:" <> show wbOps
      L.take nrOfPackets wbResults === packets

-- | Convert a list of Transactions to a list of Packets
packetFromTransactions :: forall n . KnownNat n => [Transaction 32 n (Bytes n)] -> [Packet]
packetFromTransactions [] = []
packetFromTransactions (x:xs) = case x of
  ReadSuccess _ (bitCoerce . resize . readData -> packetLength) -> packet : packetFromTransactions rest
   where
    (packetReads, rest) = L.span isRead xs
    isRead = \case
      ReadSuccess _ _ -> True
      _               -> False
    packet = L.take packetLength $ L.concatMap bytesFromTransaction packetReads
  WriteSuccess _ _ -> packetFromTransactions xs
  t -> deepErrorX $
    "packetFromTransactions: Expected ReadSuccess or WriteSuccess, but encountered" <> show t
 where
  -- Retrieve the singular bytes from a ReadSuccess Transaction.
  bytesFromTransaction :: KnownNat n => Transaction 32 n (Bytes n) -> [Unsigned 8]
  bytesFromTransaction = \case
    ReadSuccess _ (unpack . readData -> l) -> toList l
    _                                      -> []

-- | Extract a Packet by observing an Axi4 Stream.
axis4ToPackets ::
  (Show userType, KnownNat (DataWidth conf)) =>
  [Maybe (Axi4StreamM2S conf userType)] ->
  [Axi4StreamS2M] ->
  [Packet]
axis4ToPackets axisM2S axisS2M = fmap (catMaybes . L.concatMap f) packets
 where
  f Axi4StreamM2S{..} = toList (orNothing <$> _tkeep <*> _tdata)

  validMasters = mapMaybe fst (filter (\(m, s) -> isJust m && _tready s) $ L.zip axisM2S axisS2M)
  packetEnds = L.findIndices _tlast validMasters
  packets = getPackets packetEnds validMasters 0
  getPackets [] _  _ = []
  getPackets (i:iii) list  acc = pre : getPackets iii post (acc + toTake)
   where
    (pre, post) = L.splitAt toTake list
    toTake = i - acc + 1

-- | Convert a given `Packet` to a list of Wishbone master operations that write
-- the packet to the slave interface as a contiguous blob of memory with the bytes from
-- the packet arranged in big-endian format. The last operation writes the size
-- of the packet in words to the provided `packetSizeAddress`.
packetsToWb :: forall addrW nBytes . (KnownNat addrW, KnownNat nBytes) => Int -> Int -> Packet -> [WishboneM2S addrW nBytes (Bytes nBytes)]
packetsToWb packetSizeAddress packetLength allBytes = f 0 allBytes <>
  [(emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = 4 * fromIntegral packetSizeAddress
      , strobe = True
      , busCycle = True
      , writeData = fromIntegral $ ((packetLength + busWidth - 1) `div` busWidth) - 1
      , busSelect = maxBound
      , writeEnable = True}]
 where
  busWidth = natToNum @nBytes
  f i bytes =
    (emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = 4 * i
      , strobe = True
      , busCycle = True
      , writeData
      , busSelect
      , writeEnable = True} : if otherBytes == [] then [] else f (succ i) otherBytes
    where
    (bytesToSend, otherBytes) = L.splitAt busWidth bytes
    writeData = pack $ unsafeFromList (L.reverse $ L.take busWidth (bytesToSend <> L.repeat 0))
    busSelect = pack . unsafeFromList $
      L.replicate (busWidth - L.length bytesToSend) False <>
      L.replicate (L.length bytesToSend) True

-- Transform a Packet into a list of Axi Stream operations.
packetToAxiStream ::
  forall nBytes . SNat nBytes -> Packet -> [Maybe (Axi4StreamM2S (BasicAxiConfig nBytes) ())]
packetToAxiStream w@SNat !bs
  | bs /= [] = Just axis : packetToAxiStream w rest
  | otherwise  = []
  where
  busWidth = natToNum @nBytes
  (firstWords, rest) = L.splitAt busWidth bs
  word = L.take busWidth (firstWords <> L.replicate (busWidth - 1) 0)
  axis = Axi4StreamM2S
    { _tdata = unsafeFromList word
    , _tkeep = keeps
    , _tstrb = repeat True
    , _tlast = null rest
    , _tid   = 0
    , _tdest = 0
    , _tuser = deepErrorX ""
    }
  keeps = unsafeFromList $ L.replicate (L.length bs) True <> L.replicate (busWidth - L.length bs) False

-- Write a value with Wishbone to a 4 byte aligned address.
wbWrite::
  forall addrW nBytes .
  (KnownNat addrW, KnownNat nBytes) =>
  Int ->
  Bytes nBytes ->
  WishboneM2S addrW nBytes (Bytes nBytes)
wbWrite a d =
  (emptyWishboneM2S @addrW @(Bytes nBytes))
  { busCycle    = True
  , strobe      = True
  , addr        = resize . pack $ a * 4
  , writeEnable = True
  , busSelect   = maxBound
  , writeData   = d
  }

-- Read a value from a 4 byte aligned address.
wbRead::
  forall addrW nBytes .
  (KnownNat addrW, KnownNat nBytes) =>
  Int ->
  WishboneM2S addrW nBytes (Bytes nBytes)
wbRead a =
  (emptyWishboneM2S @addrW @(Bytes nBytes))
  { busCycle  = True
  , strobe    = True
  , addr      = resize . pack $ a * 4
  , busSelect = minBound
  }
