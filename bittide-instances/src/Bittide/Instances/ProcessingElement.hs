-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
module Bittide.Instances.ProcessingElement where

import Clash.Prelude

import Bittide.DoubleBufferedRam
import Bittide.Instances.Domains
import Bittide.Instances.Hacks
import Bittide.ProcessingElement
import Bittide.SharedTypes
import Bittide.Wishbone
import Data.Bifunctor
import Protocols.Wishbone

type WishboneWidth = 4
type WishboneAddrWidth = 32
type NBusses = 32
type IMemDepth = 8092
type DMemDepth = 8092

peConfigA :: PeConfig NBusses
peConfigA = PeConfig memMap (Undefined @IMemDepth) (Undefined @DMemDepth) 0x8000000

memMap :: (KnownNat addrWidth, KnownNat nSlaves ) => MemoryMap nSlaves addrWidth
memMap = iterateI (+0x10000) 0

{-# ANN processingElement32
  (Synthesize
    { t_name = "processingElement32"
    , t_inputs =
        [ PortName "clk"
        , PortName "rst"
        , PortName "fromSlaves"
        ]
    , t_output = PortProduct ""
        [ PortName "toSlaves"
        , PortName "exposedWrite"
        ]
    }
  )#-}

processingElement32 ::
  Clock Basic100 ->
  Reset Basic100 ->
  Vec (NBusses - 3) (Signal Basic100 (WishboneS2M (Bytes WishboneWidth))) ->
  ( Vec (NBusses - 3) (Signal Basic100 (WishboneM2S WishboneAddrWidth WishboneWidth (Bytes WishboneWidth)))
  , Signal Basic100 (Maybe (BitVector WishboneWidth, Bytes WishboneWidth)))
processingElement32 clk rst = withClockResetEnable clk rst enableGen
  (processingElement peConfigA)
{-# NOINLINE processingElement32 #-}

processingElement32ReducedPins ::
  Clock Basic100 -> Reset Basic100 -> Signal Basic100 Bit -> Signal Basic100 Bit
processingElement32ReducedPins clk rst bitIn = withClockResetEnable clk rst enableGen
  $ reducePinsB ( first bundle . processingElement32 clk rst) bitIn
