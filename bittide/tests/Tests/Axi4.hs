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

axi4Group :: TestTree
axi4Group = testGroup "Axi4Group"
  [ testPropertyNamed
    "Axi4Stream upscaling does not affect packet content" "axisFromByteStreamUnchangedPackets"
    axisFromByteStreamUnchangedPackets
  , testPropertyNamed
    "Axi4Stream downscaling does not affect packet content" "axisToByteStreamUnchangedPackets"
    axisToByteStreamUnchangedPackets
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
          axisS2MBig = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MSmall, axisM2SBig) =
            wcre (axisToByteStream @System @_ @(BasicAxiConfig 1)) axisM2SSmall axisS2MBig
          axisM2SSmall = wcre $ axi4StreamSimDriver axiStream axisS2MSmall
        maxSimDuration = 10 + 10*sum packetLengths * L.length repeatingReadyList
      let
        (axisM2SSmall, axisS2MSmall,axisM2SBig, axisS2MBig, _) =
          L.unzip5 $ L.takeWhile (\(_,_,_,_,r) -> r) $ sampleN maxSimDuration topEntity
        retrievedPackets = axis4ToPackets axisM2SBig axisS2MBig
      footnote . fromString $ "axisM2SSmall:" <> show axisM2SSmall
      footnote . fromString $ "axisS2MSmall:" <> show axisS2MSmall
      footnote . fromString $ "axisM2SBig:" <> show axisM2SBig
      footnote . fromString $ "axisS2MBig:" <> show axisS2MBig
      L.concat packets  === retrievedPackets


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
          axisM2SBig :: Signal System (Maybe (Axi4StreamM2S (BasicAxiConfig busWidth) () ))
          axisS2MBig = Axi4StreamS2M <$> fromList (cycle repeatingReadyList)
          (axisS2MSmall, axisM2SBig) = wcre axisFromByteStream axisM2SSmall axisS2MBig
          -- Axi stream master
          axisM2SSmall = wcre $ axi4StreamSimDriver axiStream axisS2MSmall
        maxSimDuration = 10 + 10*sum packetLengths * L.length repeatingReadyList
        (axisM2S, axisS2M, _) = L.unzip3 $ L.takeWhile (\(_,_,r) -> r) $ sampleN maxSimDuration topEntity
        retrievedPackets = axis4ToPackets axisM2S axisS2M
      L.concat packets  === retrievedPackets


-- | Extract a Packet by observing an Axi4 Stream.
axis4ToPackets ::
  KnownNat (DataWidth conf) =>
  [Maybe (Axi4StreamM2S conf userType)] ->
  [Axi4StreamS2M] ->
  Packet
axis4ToPackets axisM2S axisS2M = catMaybes . L.concat $ L.zipWith f axisM2S axisS2M
 where
  f (Just Axi4StreamM2S{..}) (_tready -> True) = toList (orNothing <$> _tkeep <*> _tdata)
  f _ _ = []

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

-- Transform a `Packet` into a list of Axi Stream operations.
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
