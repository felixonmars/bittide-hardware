-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
{-# LANGUAGE GADTs #-}

module Bittide.Node where

import Clash.Prelude
import Bittide.ProcessingElement
import Bittide.Extra.Wishbone
import Bittide.Link
import Bittide.SharedTypes
import Bittide.Switch
import Bittide.ScatterGather
import Bittide.Calendar
import Bittide.DoubleBufferedRam

-- | Each 'gppe' results in 6 busses for the 'managementUnit', namely:
-- * The 'calendar' for the 'scatterUnitWB'.
-- * The 'calendar' for the 'gatherUnitWB'.
-- * The interface of the 'rxUnit' on the 'gppe' side.
-- * The interface of the 'txUnit' on the 'gppe' side.
type BussesPerGppe = 4

-- | Each 'switch' link results in 2 busses for the 'managementUnit', namely:
-- * The interface of the 'rxUnit' on the 'switch' side.
-- * The interface of the 'txUnit' on the 'switch' side.
type BussesPerSwitchLink = 2

data NodeConfig externalLinks gppes where
  NodeConfig ::
    (KnownNat switchBusses, switchBusses ~ (1 + BussesPerSwitchLink * (externalLinks + (gppes + 1))))=>
    ManagementConfig ((BussesPerGppe * gppes) + switchBusses) ->
    SwitchConfig (externalLinks + gppes + 1) 4 32 ->
    Vec gppes GppeConfig ->
    NodeConfig externalLinks gppes

node ::
  forall dom extLinks gppes .
  (HiddenClockResetEnable dom, KnownNat extLinks, KnownNat gppes) =>
  NodeConfig extLinks gppes ->
  Vec extLinks (Signal dom (DataLink 64)) ->
  Vec extLinks (Signal dom (DataLink 64))
node (NodeConfig nmuConfig switchConfig gppeConfigs) linksIn = linksOut
 where
  (switchOut, swS2Ms) = mkswitch switchConfig swM2Ss switchIn

  switchIn = nmuToSwitch :> (pesToSwitch ++ linksIn)
  (nmuToSwitch, splitAtI -> (swM2Ss, peM2Ss)) = managementUnit nmuConfig swToNmu nmuS2Ms
  (swToNmu :> rest) = switchOut
  (switchToPes, linksOut) = splitAtI rest

  nmuS2Ms = swS2Ms ++ peS2Ms

  (pesToSwitch, concat -> peS2Ms) = unzip $ gppe <$> zip3 gppeConfigs switchToPes (unconcatI peM2Ss)

-- | Configuration for the management unit and its link.
-- The management unit contains the 4 wishbone busses that each pe has
-- and also the management busses for itself and all other pe's in this node.
-- Furthermore it also has access to the 'calendar' for the 'switch'.
data ManagementConfig nodeBusses where
  ManagementConfig ::
    (KnownNat nodeBusses) =>
    LinkConfig 4 32 ->
    PeConfig (nodeBusses + 8) ->
    ManagementConfig nodeBusses

-- | Configuration for a general purpose processing element together with its link to the
-- switch.
data GppeConfig where
  GppeConfig ::
    LinkConfig 4 32->
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
  ( GppeConfig
  , Signal dom (DataLink 64)
  , Vec 4 (Signal dom (WishboneM2S 4 32))) ->
  (Signal dom (DataLink 64), Vec 4 (Signal dom (WishboneS2M 4)))
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
  ManagementConfig nodeBusses ->
  Signal dom (DataLink 64) ->
  Vec nodeBusses  (Signal dom (WishboneS2M 4)) ->
  (Signal dom (DataLink 64), Vec nodeBusses (Signal dom (WishboneM2S 4 32)))
managementUnit (ManagementConfig linkConfig peConfig) linkIn nodeS2Ms = (linkOut, nodeM2Ss)
 where
  (suS2M, nmuS2M0) = linkToPe linkConfig linkIn sc suM2S nmuM2S0
  (linkOut, guS2M, nmuS2M1) = peToLink linkConfig sc guM2S nmuM2S1
  (suM2S :> guM2S :> rest) = nmuM2Ss
  (splitAtI -> (nmuM2S0, nmuM2S1), nodeM2Ss) = splitAtI rest
  nmuM2Ss = processingElement peConfig nmuS2Ms
  nmuS2Ms = suS2M :> guS2M :> nmuS2M0 ++ nmuS2M1 ++ nodeS2Ms
  sc = sequenceCounter
