-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Switch where

import Clash.Prelude

import Protocols.Wishbone

import Bittide.Calendar
import Bittide.Extra.Wishbone
import Bittide.Link
import Bittide.SharedTypes

-- | An index which source is selected by the crossbar, 0 selects Nothing, k selects k - 1.
type CrossbarIndex links = Index (links+1)

-- | Stores for each link, an index where the incoming frame is written to in the scatter
-- memory and a crossbar index to select the outgoing frame.
type CalendarEntry links = Vec links (CrossbarIndex links)

{-# NOINLINE switch #-}
-- | The Bittide Switch routes data from incoming to outgoing links based on a calendar.
-- The switch consists of a crossbar, a calendar and a scatter engine for all incoming links.
-- The crossbar selects one of the scatter engine outputs for every outgoing link, index 0
-- selects a null frame (Nothing) and k selects engine k - 1.
switch ::
  forall dom nBytes addrW links frameWidth preambleWidth .
  ( HiddenClockResetEnable dom
  , KnownNat addrW, 2 <= addrW
  , KnownNat frameWidth, 1 <= frameWidth
  , KnownNat links
  , KnownNat nBytes, 1 <= nBytes
  , KnownNat preambleWidth, 1 <= preambleWidth) =>
  -- | Preamble for Bittide links.
  BitVector preambleWidth ->
  -- | The calendar configuration
  CalendarConfig nBytes addrW (CalendarEntry links) ->
  -- | Wishbone interface wired to the calendar.
  Vec (1 + (2 * links)) (Signal dom (WishboneM2S addrW nBytes (Bytes nBytes))) ->
  -- | All incoming datalinks
  Vec links (Signal dom (DataLink frameWidth)) ->
  -- | All outgoing datalinks
  ( Vec links  (Signal dom (DataLink frameWidth))
  , Vec (1 + (2 * links)) (Signal dom (WishboneS2M (Bytes nBytes))))
switch preamble calConfig m2ss streamsIn = (streamsOut,calS2M :> (rxS2Ms ++ txS2Ms))
 where
  (cal, _, calS2M) = mkCalendar calConfig calM2S
  (calM2S :> (splitAtI -> (rxM2Ss, txM2Ss))) = m2ss
  sc = sequenceCounter

  rxS2Ms = rxUnit preamble sc <$> streamsIn <*> rxM2Ss
  scatterFrames = register Nothing <$> streamsIn
  crossBarOut =  unbundle $ crossBar <$> cal <*> bundle scatterFrames
  gatherFrames = register Nothing <$> crossBarOut
  (txS2Ms, streamsOut) = unzip $ txUnit preamble sc <$> gatherFrames <*> txM2Ss

{-# NOINLINE crossBar #-}
-- | The crossbar receives a vector of indices and a vector of incoming frames.
-- For each outgoing link it will select a data source. 0 selects a null frame (Nothing),
-- therefore indexing of incoming links starts at 1 (index 1 selects incoming frame 0).
-- Source: bittide hardware, switch logic.
crossBar ::
  (KnownNat links) =>
  Vec links (CrossbarIndex links) ->
  -- | Source selection for each outgoing link, 0 is a null frame, links start at index 1.
  Vec links (Maybe a) ->
  -- | Vector of incoming links.
  Vec links (Maybe a)
crossBar calendarEntry inputStreams = fmap selectChannel calendarEntry
 where
  selectChannel i = (Nothing :> inputStreams) !! i
