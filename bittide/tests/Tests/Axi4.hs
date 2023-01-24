{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Functor law" #-}
module Tests.Axi4 where

import Clash.Prelude
import Bittide.Axi4

import Clash.Hedgehog.Sized.Unsigned
import Clash.Sized.Vector(unsafeFromList)
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
    "Wishbone accessible Axis buffer can be written to and read" "wbAxisBufferReadStreams"
    wbAxisBufferReadStreams
  ]

type Packet = [Unsigned 8]

wbAxisBufferReadStreams :: Property
wbAxisBufferReadStreams = property $ do
  fifoDepth <- forAll $ Gen.enum 1 8
  bytesInWord <- forAll $ Gen.enum 1 8
  nrOfPackets <- forAll $ Gen.enum 0 8
  packetLengths <- forAll $
    Gen.list (Range.singleton nrOfPackets) $
    Gen.enum 1 (bytesInWord * fifoDepth)

  -- Generate list of packets [[Unsigned 8]]
  packets <- forAll $ traverse
    (flip Gen.list (genUnsigned @_ @8 Range.constantBounded) . Range.singleton)
    packetLengths
  -- Introduce fifoDepth and bytesInWord on type level
  case (TN.someNatVal $ fromIntegral (fifoDepth - 1), TN.someNatVal $ fromIntegral (bytesInWord - 1)) of
    ( SomeNat (succSNat . snatProxy -> depthSNat@SNat :: SNat fifoDepth)
     ,SomeNat (succSNat . snatProxy -> bytesSNat :: SNat bytesInWord)) -> do
      let
        -- First element in the Axi4Stream is a Nothing, this will be present during and
        -- immediately after reset.

        -- Convert the packets to [Maybe (Axi4StreamM2S)]
        axiStream = Nothing : L.concatMap (packetToAxiStream bytesSNat) packets

        -- For each packet, we first read the packet length at address fifoDepth
        -- (fifo elements can be read from address [0..(fifoDepth -1)])
        -- After that we read where we expect the data.(*)
        -- After reading all the packets, we set the packet length to 0 and clear the
        -- end of packet and buffer full flag, allowing the buffer to accept a new packet.
        -- We also start each packet reading attempt with an idle cycle to wait until
        -- a packet is ready.
        wbReadPacket pl = unpack 0 :
          (wbRead @32 @bytesInWord <$> fifoDepth : [0..(pl `div` bytesInWord)]) <>
          [wbWrite @32 @bytesInWord fifoDepth 0, wbWrite (fifoDepth + 1) maxBound]

        -- Make wishbone operations for all packets.
        wbOps = PL.concatMap wbReadPacket packetLengths
        topEntity = bundle (wbM2S, wbS2M, axisM2S, axisS2M, wbStatus, simRunning)
         where
          -- Run the simulation untill there are no more axi stream operations and
          -- the fifo is empty, run for atleast 10 cycles.
          simRunning =
            uncurry (||) <$> wbStatus
            .||. isJust <$> axisM2S
            .||. unsafeToHighPolarity (resetGenN d10)

          (wbS2M, axisS2M, wbStatus) = wcre $
            wbAxisRxBuffer @System @32 @bytesInWord
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
    bytesInWord = natToNum @nBytes
    (firstWords, rest) = L.splitAt bytesInWord bs
    word = L.take bytesInWord (firstWords <> L.replicate (bytesInWord - 1) 0)
    axis = Axi4StreamM2S
      { _tdata = unsafeFromList word
      , _tkeep = unpack keeps
      , _tstrb = repeat True
      , _tlast = null rest
      , _tid   = 0
      , _tdest = 0
      , _tuser = deepErrorX ""
      }
    keeps = case L.length bs of
      7         -> 0b01111111
      6         -> 0b00111111
      5         -> 0b00011111
      4         -> 0b00001111
      3         -> 0b00000111
      2         -> 0b00000011
      1         -> 0b00000001
      0         -> 0b00000000
      _         -> 0b11111111

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
