-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bittide.Link where

import Clash.Prelude

import Data.Constraint
import Data.Constraint.Nat.Extra
import Data.Maybe
import Protocols.Wishbone

import Bittide.DoubleBufferedRam
import Bittide.SharedTypes


-- Internal states of the txUnit.
data TransmissionState preambleWidth seqCountWidth frameWidth
  = LinkThrough
  -- ^ The txUnit is transparent, the incoming frame is directly routed to the output.
  | TransmitPreamble (Index (Regs (BitVector preambleWidth) frameWidth))
  -- ^ The txUnit is transmitting the preamble, the index keeps track of which frame of
  -- the preamble is being transmitted.
  | TransmitSeqCounter (Index (DivRU seqCountWidth frameWidth))
  -- ^ The txUnit is transmitting the stored sequence counter, the index keeps track
  -- of which frame of the sequence counter is being transmitted.
   deriving (Generic, NFDataX)

{-# NOINLINE txUnit #-}
-- | Transmitter for the Bittide Link, it either transmits the incoming gather frame or
-- transmits the preamble followed by the sequence counter.
txUnit ::
  forall core nBytes aw preambleWidth frameWidth seqCountWidth .
  ( HiddenClockResetEnable core
  , KnownNat preambleWidth
  , KnownNat seqCountWidth
  , KnownNat frameWidth
  , KnownNat nBytes
  , 1 <= nBytes
  , KnownNat aw
  , 2 <= aw
  , 1 <= frameWidth) =>
  -- |  Hardcoded preamble.
  BitVector preambleWidth ->
  -- | Local sequence counter.
  Signal core (Unsigned seqCountWidth) ->
  -- | Frame from 'gatherUnitWb'
  Signal core (DataLink frameWidth) ->
  -- | Control register Wishbone bus (Master -> slave).
  Signal core (WishboneM2S aw nBytes (Bytes nBytes)) ->
  -- |
  -- 1. Control register Wishbone bus (Slave -> master).
  -- 2. Outgoing frame
  ( Signal core (WishboneS2M (Bytes nBytes))
  , Signal core (DataLink frameWidth))
txUnit (getRegsBe -> RegisterBank preamble) sq frameIn wbIn = (wbOut, frameOut)
 where
  (stateMachineOn, wbOut)
    | Dict <- timesDivRU @(nBytes * 8) @1
    = registerWb WishbonePriority False wbIn (pure Nothing)

  frameOut = withReset regReset $ mealy stateMachine (scErr, LinkThrough) mealyIn
  mealyIn = bundle (frameIn, sq)
  regReset = forceReset $ not <$> stateMachineOn

  stateMachine ::
    (Unsigned seqCountWidth, TransmissionState preambleWidth seqCountWidth frameWidth) ->
    (DataLink frameWidth, Unsigned seqCountWidth) ->
    ( (Unsigned seqCountWidth
    , TransmissionState preambleWidth seqCountWidth frameWidth)
    , DataLink frameWidth)
  stateMachine (scStored@(getRegsBe -> RegisterBank sqVec), state) (fIn, scIn) =
    ((nextSc, nextState state), out)
   where
    (nextSc, out) = case state of
      LinkThrough          -> (scStored, fIn)
      TransmitSeqCounter n -> (scStored, Just $ sqVec !! n)
      TransmitPreamble n
        | n == maxBound -> (scIn,     Just $ preamble !! n)
        | otherwise     -> (scStored, Just $ preamble !! n)
  scErr = deepErrorX "txUnit: Stored sequence counter invalid"

  -- Once turned on, the txUnit continues to transmit the preamble followed by the sequence
  -- counter.
  nextState = \case
      LinkThrough       -> TransmitPreamble 0
      TransmitPreamble n
        | n == maxBound -> TransmitSeqCounter 0
        | otherwise     -> TransmitPreamble (succ n)
      TransmitSeqCounter n
        | n == maxBound -> TransmitPreamble 0
        | otherwise     -> TransmitSeqCounter (succ n)

-- | States for the rxUnit.
data ReceiverState
  = Empty
  -- ^ Receiver is in idle state.
  | WaitingForPreamble
  -- ^ Receiver is waiting for the preamble to be detected.
  | CaptureSequenceCounter
  -- ^ Receiver is capturing the sequence counter.
  | Done
  -- ^ Receiver has captured a remote and corresponding local sequence counter.
  deriving (Generic, ShowX, BitPack, NFDataX)

-- | We store the remote sequence counter, local sequence counter and 'ReceiverState'
-- as a vector of words to make sure they are word-aligned.
type RxRegister nBytes scw =
  Vec
  ( Regs (Unsigned scw) (nBytes * 8)
  + Regs (Unsigned scw) (nBytes * 8)
  + Regs ReceiverState (nBytes * 8)
  )
  (BitVector (nBytes * 8))

{-# NOINLINE rxUnit #-}
-- | Receives a Bittide link and can be set to detect the given preamble and capture the
-- following sequence counter.
rxUnit ::
  forall core nBytes aw paw fw scw .
  ( HiddenClockResetEnable core
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat aw, 2 <= aw
  , KnownNat paw, 1 <= paw
  , KnownNat fw, 1 <= fw
  , KnownNat scw, 1 <= scw) =>
  -- | Preamble.
  BitVector paw ->
  -- | Local sequence counter.
  Signal core (Unsigned scw) ->
  -- | Incoming bittide link.
  Signal core (DataLink fw) ->
  -- | Control register Wishbone bus (Master -> slave).
  Signal core (WishboneM2S aw nBytes (Bytes nBytes)) ->
  -- | Control register Wishbone bus (Slave -> master).
  Signal core (WishboneS2M (Bytes nBytes))
rxUnit preamble localCounter linkIn wbIn = wbOut
 where
  (regOut, wbOut) = registerWbE WishbonePriority regInit wbIn regIn (pure maxBound)
  regInit = mkWordAligned (0 :: Unsigned scw, 0 :: Unsigned scw, Empty)
  regIn = unbundle . mealy go
    (0,0) $ bundle
    ( fmap fromWordAligned regOut, linkIn, localCounter)

  go ::
    (Index (DivRU scw fw), BitVector paw) ->
    ( (Unsigned scw, Unsigned scw, ReceiverState)
    , DataLink fw, Unsigned scw) ->
    ( (Index (DivRU scw fw), BitVector paw)
    , Maybe (RxRegister nBytes scw)
    )
  go
    (count, shiftOld)
    ( ( remoteSc0
      , localSc0
      , state
      )
    , link
    , localSc1) =
    ( (nextCount, shiftNext), mkWordAligned <$> wbRegNew)
   where
    (remoteSc1, RegisterBank remoteFrames0) = convertBe (RegisterBank newVec, remoteSc0)
     where
      newVec = tail $ remoteFrames0 :< fromJust link
    (shiftNew, RegisterBank oldVec) = convertBe (RegisterBank newVec, shiftOld)
     where
      newVec = tail $ oldVec :< fromJust link

    preambleFound = validFrame && shiftNew == preamble

    validFrame = isJust link
    firstFrame = validFrame && count == minBound
    lastFrame  = validFrame && count == maxBound

    shiftNext = case (validFrame, state) of
      (True, WaitingForPreamble) -> shiftNew
      _                          -> shiftOld

    wbRegNew = case (state, validFrame, firstFrame) of
      (WaitingForPreamble    ,True,_)    -> Just (remoteSc0, localSc0, nextState)
      (CaptureSequenceCounter,True,True) -> Just (remoteSc1, localSc1, nextState)
      (CaptureSequenceCounter,True,_)    -> Just (remoteSc1, localSc0, nextState)
      _                                  -> Nothing

    (nextState, nextCount) = case (preambleFound, lastFrame , state) of
      (False, _    , WaitingForPreamble)    -> (WaitingForPreamble    , 0)
      (True , _    , WaitingForPreamble)    -> (CaptureSequenceCounter, 0)
      (_    , False, CaptureSequenceCounter)-> (CaptureSequenceCounter, succ count)
      (_    , True , CaptureSequenceCounter)-> (Done                  , 0)
      _                                     -> (state                 , 0)

  mkWordAligned ::
    forall wordSize a b c .
    (KnownNat wordSize, Paddable a, Paddable b, Paddable c) =>
    (a,b,c) ->
    Vec (Regs a wordSize + Regs b wordSize + Regs c wordSize) (BitVector wordSize)
  mkWordAligned (a,b,c) = regsA ++ regsB ++ regsC
   where
    RegisterBank regsA = getRegsBe a
    RegisterBank regsB = getRegsBe b
    RegisterBank regsC = getRegsBe c

  fromWordAligned ::
    forall wordSize a b c .
    (KnownNat wordSize, 1 <= wordSize, Paddable a, Paddable b, Paddable c) =>
    Vec (Regs a wordSize + Regs b wordSize + Regs c wordSize) (BitVector wordSize) ->
    (a,b,c)
  fromWordAligned vec = (a,b,c)
   where
    (vecA, splitAtI -> (vecB, vecC)) = splitAtI vec
    a = getDataBe (RegisterBank vecA)
    b = getDataBe (RegisterBank vecB)
    c = getDataBe (RegisterBank vecC)

-- | Counts the number of cycles since the last reset. Initially Unsigned 64 has been
--  picked because it's unlikely to overflow in the lifetime of a Bittide system.
sequenceCounter :: HiddenClockResetEnable dom => Signal dom (Unsigned 64)
sequenceCounter = register 0 $ satSucc SatError <$> sequenceCounter
