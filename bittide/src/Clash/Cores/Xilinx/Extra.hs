-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.Xilinx.Extra
  ( ibufds
  , module GTH

  -- * Internal
  , ibufdsTF
  ) where

import Clash.Prelude

import Data.String.Interpolate (__i)

import Control.Monad.State (State)
import Clash.Annotations.Primitive
import Clash.Backend (Backend)
import Clash.Netlist.Types
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Text.Show.Pretty (ppShow)

import Clash.Cores.Xilinx.GTH as GTH

import qualified Clash.Netlist.Id as Id
import qualified Clash.Primitives.DSL as DSL
import qualified Prelude as P

-- | A differential input buffer. For more information see:
--
--     https://docs.xilinx.com/r/en-US/ug974-vivado-ultrascale-libraries/IBUFDS
--
ibufds :: (KnownDomain dom)  => DiffClock dom -> Clock dom
ibufds !_ = clockGen
{-# ANN ibufds hasBlackBox #-}
{-# NOINLINE ibufds #-}
{-# ANN ibufds (InlineYamlPrimitive [minBound..] [__i|
  BlackBox:
    name: Clash.Cores.Xilinx.Extra.ibufds
    kind: Declaration
    format: Haskell
    templateFunction: Clash.Cores.Xilinx.Extra.ibufdsTF
  |]) #-}

-- | Template function for 'ibufds'.
--
-- TODO: Upstream to @clash-cores@
ibufdsTF :: TemplateFunction
ibufdsTF = TemplateFunction used valid go
 where
  used = [0, 1]
  valid = const True

  go :: Backend s => BlackBoxContext -> State s Doc
  go bbCtx
    | [ _knownDomain, clk] <- P.map fst (DSL.tInputs bbCtx)
    , DataCon (Product "Clash.Signal.Internal.DiffClock" _ clkTys) _ clkEs <- DSL.eex clk
    , [clkP@(Identifier _ Nothing), clkN@(Identifier _ Nothing)] <- clkEs
    , [clkPTy, clkNTy] <- clkTys
    = do
        instLabel <- Id.makeBasic "ibufds_inst"

        DSL.declarationReturn bbCtx "ibufds_block" $ do
          ibufdsOut <- DSL.declare "ibufds_out" Bit

          let
            compName = "IBUFDS"
            compInps = [("I", Bit), ("IB", Bit)]
            compOuts = [("O", Bit)]
            inps = [("I", DSL.TExpr clkPTy clkP), ("IB", DSL.TExpr clkNTy clkN)]
            outs = [("O", ibufdsOut)]


          DSL.compInBlock compName compInps compOuts
          DSL.instDecl Empty (Id.unsafeMake compName) instLabel [] inps outs

          pure [ibufdsOut]


  go bbCtx = error ("ibufdsTemplate:\n\n" <> ppShow bbCtx)
