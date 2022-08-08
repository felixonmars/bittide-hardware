-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# LANGUAGE GADTs #-}

module Bittide.Node where

import Clash.Prelude

import Protocols.Wishbone

import Bittide.Link
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Switch

data NodeConfig externalLinks gppes where
  NodeConfig ::
    ( KnownNat switchBusses
    , switchBusses ~ (1 + 2 * (externalLinks + (gppes + 1))))=>
    ManagementConfig ((4 * gppes) + switchBusses) ->
    SwitchConfig (externalLinks + gppes + 1) 4 32 ->
    Vec gppes GppeConfig ->
    NodeConfig externalLinks gppes

node ::
  forall dom extLinks gppes .
  ( HiddenClockResetEnable dom, KnownNat extLinks, KnownNat gppes) =>
  NodeConfig extLinks gppes ->
  Vec extLinks (Signal dom (DataLink 64)) ->
  Vec extLinks (Signal dom (DataLink 64))
node (NodeConfig nmuConfig switchConfig gppeConfigs) linksIn = linksOut
 where
  (switchOut, swS2Ms) =
    mkSwitch switchConfig swCalM2S swRxM2Ss swTxM2Ss switchIn
  switchIn = nmuToSwitch :> pesToSwitch ++ linksIn
  (splitAtI -> (switchToNmu :> switchToPes, linksOut)) = switchOut
  (nmuToSwitch, nmuM2Ss) = managementUnit nmuConfig switchToNmu nmuS2Ms
  (swM2Ss, peM2Ss) = splitAtI nmuM2Ss

  (swCalM2S :> swRxM2Ss, swTxM2Ss) = splitAtI swM2Ss
  (swCalS2M :> swRxS2Ms, swTxS2Ms) = splitAtI @(1 + (extLinks + (gppes + 1))) @(extLinks + (gppes + 1)) swS2Ms

  nmuS2Ms = swCalS2M :> (swRxS2Ms ++ swTxS2Ms ++ peS2Ms)

  (pesToSwitch, concat -> peS2Ms) = unzip $ gppe <$> zip3 gppeConfigs switchToPes (unconcatI peM2Ss)

-- | Configuration for the 'managementUnit' and its 'Bittide.Link'.
-- The management unit contains the 4 wishbone busses that each pe has
-- and also the management busses for itself and all other pe's in this node.
-- Furthermore it also has access to the 'calendar' for the 'switch'.
data ManagementConfig nodeBusses where
  ManagementConfig ::
    (KnownNat nodeBusses) =>
    -- | Configuration for the incoming and outgoing 'Bittide.Link'.
    LinkConfig 4 32 ->
    -- | Configuration for the 'managementUnit's 'processingElement'. Controls 8 local busses
    -- and all incoming busses from 'calendar's, 'rxUnit's and 'txUnit's.
    PeConfig (nodeBusses + 8) ->
    ManagementConfig nodeBusses

-- | Configuration for a general purpose processing element together with its link to the
-- switch.
data GppeConfig where
  GppeConfig ::
    LinkConfig 4 32->
    -- | Configuration for a 'gppe's 'processingElement', which statically
    -- has four external busses connected to the instruction memory, data memory
    -- , 'scatterUnitWb' and 'gatherUnitWb'.
    PeConfig 4 ->
    GppeConfig

{-# NOINLINE gppe #-}

-- | A general purpose 'processingElement' to be part of a Bittide Node. It contains
-- a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
-- Bittide Link. It takes a 'GppeConfig', incoming link and four incoming 'WishboneM2S'
-- signals and produces the outgoing link alongside four 'WishhboneS2M' signals.
-- The order of Wishbone busses is as follows:
-- ('rxUnit' :> 'scatterUnitWb' :> 'txUnit' :> 'gatherUnitWb' :> Nil).
gppe ::
  HiddenClockResetEnable dom =>
  -- |
  -- ( Configures all local parameters
  -- , Incoming 'Bittide.Link'
  -- , Incoming @Vector@ of master busses
  -- )
  ( GppeConfig
  , Signal dom (DataLink 64)
  , Vec 4 (Signal dom (WishboneM2S 32 4 (Bytes 4)))) ->
  -- |
  -- ( Outgoing 'Bittide.Link'
  -- , Outgoing @Vector@ of slave busses
  -- )
  ( Signal dom (DataLink 64)
  , Vec 4 (Signal dom (WishboneS2M (Bytes 4))))
gppe (GppeConfig linkConfig peConfig, linkIn, splitAtI -> (nmuM2S0, nmuM2S1)) =
  (linkOut, nmuS2M0 ++ nmuS2M1)
 where
  (suS2M, nmuS2M0) = linkToPe linkConfig linkIn sc suM2S nmuM2S0
  (linkOut, guS2M, nmuS2M1) = peToLink linkConfig sc guM2S nmuM2S1
  (suM2S :> guM2S :> Nil) = processingElement peConfig (suS2M :> guS2M :> Nil)
  sc = sequenceCounter

{-# NOINLINE managementUnit #-}

-- | A special purpose 'processingElement' that manages a Bittide Node. It contains
-- a 'processingElement', 'linkToPe' and 'peToLink' which create the interface for the
-- Bittide Link. It takes a 'ManagementConfig', incoming link and a vector of incoming
-- 'WishboneS2M' signals and produces the outgoing link alongside a vector of
-- 'WishhboneM2S' signals.
managementUnit ::
  forall dom nodeBusses .
  (HiddenClockResetEnable dom, KnownNat nodeBusses) =>
  -- | Configures all local parameters.
  ManagementConfig nodeBusses ->
  -- | Incoming 'Bittide.Link'.
  Signal dom (DataLink 64) ->
  -- | Incoming @Vector@ of slave busses.
  Vec nodeBusses  (Signal dom (WishboneS2M (Bytes 4))) ->
  -- |
  -- ( Outgoing 'Bittide.Link'
  -- , Outgoing @Vector@ of master busses)
  ( Signal dom (DataLink 64)
  , Vec nodeBusses (Signal dom (WishboneM2S 32 4 (Bytes 4))))
managementUnit (ManagementConfig linkConfig peConfig) linkIn nodeS2Ms = (linkOut, nodeM2Ss)
 where
  (suS2M, nmuS2M0) = linkToPe linkConfig linkIn sc suM2S nmuM2S0
  (linkOut, guS2M, nmuS2M1) = peToLink linkConfig sc guM2S nmuM2S1
  (suM2S :> guM2S :> rest) = nmuM2Ss
  (splitAtI -> (nmuM2S0, nmuM2S1), nodeM2Ss) = splitAtI rest
  nmuM2Ss = processingElement peConfig nmuS2Ms
  nmuS2Ms = suS2M :> guS2M :> nmuS2M0 ++ nmuS2M1 ++ nodeS2Ms
  sc = sequenceCounter
