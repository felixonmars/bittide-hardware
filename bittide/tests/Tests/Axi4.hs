{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.Axi4 where

import Clash.Prelude
import Bittide.Axi4

import Clash.Hedgehog.Sized.Unsigned
import Clash.Sized.Vector(unsafeFromList)
import Clash.Hedgehog.Sized.Vector (genVec)
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tests.Shared
import Protocols.Axi4.Stream
import Data.Maybe
import qualified GHC.TypeNats as TN
import qualified Data.List as L
import qualified Prelude as PL
import Protocols.Wishbone
import Bittide.SharedTypes
import Data.String

axi4Group :: TestTree
axi4Group = testGroup "Axi4Group"
  [ testPropertyNamed
    "Wishbone accessible Axis receive buffer can be written to and read: wbAxisRxBufferReadStreams" "wbAxisRxBufferReadStreams"
    wbAxisRxBufferReadStreams
  , testPropertyNamed
    "Wishbone accessible Axis transmit buffer can be written to and read" "wbAxisTxBufferWriteStreams"
    wbAxisTxBufferWriteStreams
  , testPropertyNamed
    "Axi4Stream upscaling does not affect packet content" "axisFromByteStreamUnchangedPackets"
    axisFromByteStreamUnchangedPackets
  , testPropertyNamed
    "Axi4Stream downscaling does not affect packet content" "axisToByteStreamUnchangedPackets"
    axisToByteStreamUnchangedPackets
  ]

type Packet = [Unsigned 8]
type Axi4TestConfig dw id dest= 'Axi4StreamConfig dw id dest

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


axisToByteStreamUnchangedPackets :: Property
axisToByteStreamUnchangedPackets = property $ do
  busWidth <- forAll $ Gen.enum 1 8
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
          axisM2SBig :: Signal System (Maybe (Axi4StreamM2S (BasicAxiConfig 1) () ))
          axisS2MBig = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MSmall, axisM2SBig) = wcre axisToByteStream axisM2SSmall axisS2MBig
          -- Axi stream master
          axisM2SSmall = withClockResetEnable @System clockGen resetGen (toEnable nextAxi)
            $ fromListWithControl (axiStream <> L.repeat Nothing)
          -- Axi Stream backpressure by disabling fromListWithControl
          nextAxi = axisS2MSmall .==. pure (Axi4StreamS2M True)
        (axisM2SSmall, axisS2MSmall,axisM2SBig, axisS2MBig, _) = L.unzip5 $ L.takeWhile (\(_,_,_,_,r) -> r) $ sampleN 2000 topEntity
        retrievedPackets = axis4ToPackets axisM2SBig axisS2MBig
      footnote . fromString $ "axisM2SSmall:" <> show axisM2SSmall
      footnote . fromString $ "axisS2MSmall:" <> show axisS2MSmall
      footnote . fromString $ "axisM2SBig:" <> show axisM2SBig
      footnote . fromString $ "axisS2MBig:" <> show axisS2MBig
      L.concat packets  === retrievedPackets


axisFromByteStreamUnchangedPackets :: Property
axisFromByteStreamUnchangedPackets = property $ do
  busWidth <- forAll $ Gen.enum 1 8
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
          axisM2SBig :: Signal System (Maybe (Axi4StreamM2S (BasicAxiConfig busWidth) () ))
          axisS2MBig = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MSmall, axisM2SBig) = wcre axisFromByteStream axisM2SSmall axisS2MBig
          -- Axi stream master
          axisM2SSmall = withClockResetEnable @System clockGen resetGen (toEnable nextAxi)
            $ fromListWithControl (axiStream <> L.repeat Nothing)
          -- Axi Stream backpressure by disabling fromListWithControl
          nextAxi = axisS2MSmall .==. pure (Axi4StreamS2M True)
        (axisM2S, axisS2M, _) = L.unzip3 $ L.takeWhile (\(_,_,r) -> r) $ sampleN 2000 topEntity
        retrievedPackets = axis4ToPackets axisM2S axisS2M
      L.concat packets  === retrievedPackets

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
          nextWb =
            (\WishboneM2S{strobe, busCycle}
              WishboneS2M{acknowledge, err} ->
                not (strobe && busCycle) || acknowledge || err)
            <$> wbM2S <*> wbS2M
          (wbS2M, axisM2S) = wcre $
            wbAxisTxBuffer @System @32 @busWidth @fifoDepth @(BasicAxiConfig busWidth)
            SNat wbM2S axisS2M
          axisS2M = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          wbM2S = withClockResetEnable @System clockGen resetGen (toEnable nextWb)
            $ fromListWithControl (unpack 0 : wbOps <> L.repeat (unpack 0))
          simRunning = not <$> (isNothing <$> axisM2S .&&. wbInactive .&&. streamingStarted)
          streamingStarted = wcre $ register False (streamingStarted .||. isJust <$> axisM2S)
          wbInactive = (\WishboneM2S{..} -> not $ busCycle || strobe) <$> wbM2S
        (axisM2SList, axisS2MList, _) = L.unzip3 . L.takeWhile (\(_,_,r) -> r)
          $ sampleN (10 + fifoDepth * nrOfPackets *  L.length repeatingReadyList + L.length wbOps) topEntity

      footnote . fromString $ "wbOps:" <> show wbOps
      footnote . fromString $ "axisM2SList:" <> show axisM2SList
      footnote . fromString $ "axisS2MList:" <> show axisS2MList
      L.concat packets === axis4ToPackets axisM2SList axisS2MList
axis4ToPackets :: KnownNat (DataWidth conf) =>
  [Maybe (Axi4StreamM2S conf userType)] ->
  [Axi4StreamS2M] ->
  [Unsigned 8]
axis4ToPackets axisM2S axisS2M = catMaybes . L.concat $ L.zipWith f axisM2S axisS2M
 where
  f (Just Axi4StreamM2S{..}) (_tready -> True) =
    toList ((\ a b -> if a then Just b else Nothing) <$> _tkeep <*> _tdata)
  f _ _ = []
packetsToWb :: forall addrW nBytes . (KnownNat addrW, KnownNat nBytes) => Int -> Int -> Packet -> [WishboneM2S addrW nBytes (Bytes nBytes)]
packetsToWb fifoDepth packetLength allBytes = f 0 allBytes <>
  [(emptyWishboneM2S @addrW @(Bytes nBytes))
      { addr = 4 * fromIntegral fifoDepth
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
        -- First element in the Axi4Stream is a Nothing, this will be present during and
        -- immediately after reset.

        -- Convert the packets to [Maybe (Axi4StreamM2S)]
        axiStream = Nothing : L.concatMap (packetToAxiStream busWidthSNat) packets

        -- For each packet, we first read the packet length at address fifoDepth
        -- (fifo elements can be read from address [0..(fifoDepth -1)])
        -- After that we read where we expect the data.(*)
        -- After reading all the packets, we set the packet length to 0 and clear the
        -- end of packet and buffer full flag, allowing the buffer to accept a new packet.
        -- We also start each packet reading attempt with an idle cycle to wait until
        -- a packet is ready.
        wbReadPacket pl = unpack 0 :
          (wbRead @32 @busWidth <$> fifoDepth : [0..(pl `div` busWidth)]) <>
          [wbWrite @32 @busWidth fifoDepth 0, wbWrite (fifoDepth + 1) maxBound]

        -- Make wishbone operations for all packets.
        wbOps = PL.concatMap wbReadPacket packetLengths
        topEntity = bundle (wbM2S, wbS2M, axisM2S, axisS2M, wbStatus, simRunning)
         where
          -- Run the simulation until there are no more axi stream operations and
          -- the fifo is empty, run for at least 10 cycles.
          simRunning =
            uncurry (||) <$> wbStatus
            .||. isJust <$> axisM2S
            .||. unsafeToHighPolarity (resetGenN d10)

          (wbS2M, axisS2M, wbStatus) = wcre $
            wbAxisRxBuffer @System @32 @busWidth
            depthSNat wbM2S axisM2S (pure (False, False))

          -- Axi stream master
          axisM2S = withClockResetEnable @System clockGen resetGen (toEnable nextAxi)
            $ fromListWithControl (axiStream <> L.repeat Nothing)

          -- Wishbone master
          wbM2S = withClockResetEnable @System clockGen resetGen (toEnable nextWb)
            $ fromListWithControl (wbOps <> L.repeat (unpack 0))

          wbMasterActive = busCycle <$> wbM2S .&&. strobe <$> wbM2S
          -- Wishbone backpressure by disabling fromListWithControl
          nextWb = uncurry (||) <$> wbStatus .&&. (not <$> wbMasterActive .||. acknowledge <$> wbS2M)
          -- Axi Stream backpressure by disabling fromListWithControl
          nextAxi = axisS2M .==. pure (Axi4StreamS2M True)

        -- Run until the simulation is done.
        simOut = L.takeWhile (\(_,_,_,_,_,simRunning) -> simRunning) $ sample topEntity
        (m, s, _, _, _,_) = L.unzip6 simOut
        -- Transform the wishbone signals into Transactions and into Packets
        wbResults = packetFromTransactions $ wbToTransaction m s
      footnote . fromString $ "(wbM2S, wbS2M, axisM2S, axisS2M, wbStatus, simRunning): " <> show simOut
      footnote . fromString $ "wbOps:" <> show wbOps
      L.take nrOfPackets wbResults === packets
 where
  -- Convert a list of Transactions to a list of Packets
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

  -- Retrieve the singular bytes from a ReadSuccess Transaction.
  bytesFromTransaction :: KnownNat n => Transaction 32 n (Bytes n) -> [Unsigned 8]
  bytesFromTransaction = \case
    ReadSuccess _ (unpack . readData -> l) -> toList l
    _                                      -> []

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
wbWrite:: forall addrW nBytes . (KnownNat addrW, KnownNat nBytes) => Int -> Bytes nBytes -> WishboneM2S addrW nBytes (Bytes nBytes)
wbWrite a d = (emptyWishboneM2S @addrW @(Bytes nBytes))
          { busCycle  = True
          , strobe    = True
          , addr      = resize . pack $ a * 4
          , writeEnable = True
          , busSelect = maxBound
          , writeData = d
          }

-- Read a value from a 4 byte aligned address.
wbRead:: forall addrW nBytes . (KnownNat addrW, KnownNat nBytes) => Int -> WishboneM2S addrW nBytes (Bytes nBytes)
wbRead a = (emptyWishboneM2S @addrW @(Bytes nBytes))
          { busCycle  = True
          , strobe    = True
          , addr      = resize . pack $ a * 4
          , busSelect = minBound
          }
type BasicAxiConfig nBytes = 'Axi4StreamConfig nBytes 0 0
