-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.GTH.BlackBoxes where
import Prelude

import Control.Monad.State (State)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Extra (Doc)
import GHC.Stack (HasCallStack)

import Clash.Backend (Backend)
import Clash.Netlist.BlackBox.Types (BlackBoxFunction, emptyBlackBoxMeta)
import Clash.Netlist.Types (TemplateFunction(..), BlackBox(BBFunction), BlackBoxContext)

import qualified Clash.Primitives.DSL as DSL
import qualified Clash.Netlist.Id as Id
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.BlackBox.Types as N
import Clash.Netlist.BlackBox.Util (exprToString)

import Clash.Cores.Xilinx.Internal
  (defIpConfig, IpConfig(properties), property, renderTcl, TclPurpose(..), BraceTcl(..))
import Data.String (fromString)

gthCoreBBF :: HasCallStack => BlackBoxFunction
gthCoreBBF _isD _primName _args _resTys
  =
 let
  bbMeta = emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes =
      [ ( ("gth", "clash.tcl")
        , BBFunction (show 'gthCoreTclTF) 0 gthCoreTclTF)
      ]
    }

  bb :: BlackBox
  bb = BBFunction (show 'gthCoreTF) 0 gthCoreTF
 in
  pure $ Right (bbMeta, bb)

-- | Instantiate IP generated with 'gthCoreTclTF'
gthCoreTF :: HasCallStack => TemplateFunction
gthCoreTF =
  TemplateFunction
    [0..10]
    (const True)
    gthCoreBBTF

gthCoreBBTF ::
  Backend s =>
  BlackBoxContext ->
  State s Doc
gthCoreBBTF bbCtx
  | args@[
      _gthrxn_in  -- " ::: Signal rxS (BitVector ChansUsed)
    , _gthrxp_in  -- " ::: Signal rxS (BitVector ChansUsed)
    , _gtwiz_reset_clk_freerun_in  -- " ::: Clock freerun
    , _gtwiz_reset_all_in  -- " ::: Reset freerun
    , _gtwiz_reset_tx_pll_and_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_tx_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_rx_pll_and_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_rx_datapath_in  -- " ::: Reset freerun
    , _gtwiz_userdata_tx_in  -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH))
    , _txctrl2_in  -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH/8))
    -- , _gtrefclk00_in  -- " ::: Clock refclk00
    , _drpclk_in  -- " ::: Clock freerun
    , _gtrefclk0_in  -- " ::: Clock refclk0
    ] <- drop 2 $ map fst (DSL.tInputs bbCtx)
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  , [gthCoreName] <- N.bbQsysIncName bbCtx
  = do

  gthCoreInstName <- Id.makeBasic "gthcore_inst"

  let
    chansUsed = 1
    tX_DATA_WIDTH = 64
    rX_DATA_WIDTH = tX_DATA_WIDTH
    compInps =
      [
        ("gthrxn_in",  N.BitVector chansUsed)
      , ("gthrxp_in",  N.BitVector chansUsed)
      , ("gtwiz_reset_clk_freerun_in",  N.Clock "freerun" )
      , ("gtwiz_reset_all_in",  N.Reset "freerun" )

      , ("gtwiz_reset_tx_pll_and_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_tx_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_rx_pll_and_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_rx_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_userdata_tx_in",  N.BitVector (chansUsed*tX_DATA_WIDTH))
      , ("txctrl2_in",  N.BitVector (chansUsed*(tX_DATA_WIDTH `div` 8)))

      -- , ("gtrefclk00_in",  N.Clock "refclk00" )
      , ("drpclk_in",  N.Clock "freerun" )
      , ("gtrefclk0_in",  N.Clock "refclk0" )
      ] <> map (fmap DSL.ety) tiedOffInps
    tiedOffInps =
      [
        ("txctrl0_in", DSL.bvLit 16 0)
      , ("txctrl1_in", DSL.bvLit 16 0)

      , ("tx8b10ben_in", DSL.bvLit  1 1)
      , ("rx8b10ben_in", DSL.bvLit  1 1)

      , ("gtwiz_userclk_tx_reset_in", DSL.bvLit  1 0)
      , ("gtwiz_userclk_rx_reset_in", DSL.bvLit  1 0)

      , ("rxcommadeten_in", DSL.bvLit  1 1)
      , ("rxmcommaalignen_in", DSL.bvLit  1 1)
      , ("rxpcommaalignen_in", DSL.bvLit  1 1)
      ]
    compOuts =
      [
        ("gthtxn_out",  N.BitVector chansUsed)
      , ("gthtxp_out",  N.BitVector chansUsed)
      , ("gtwiz_userclk_tx_usrclk2_out",  N.Clock "txUser2")
      , ("gtwiz_userclk_rx_usrclk2_out",  N.Clock "rxUser2")
      , ("gtwiz_userdata_rx_out",  N.BitVector (chansUsed*rX_DATA_WIDTH))
      , ("gtwiz_reset_tx_done_out", N.BitVector 1)
      , ("gtwiz_reset_rx_done_out", N.BitVector 1)

      , ("gtwiz_userclk_tx_active_out", N.BitVector 1)
      ]

  DSL.declarationReturn bbCtx "gthCore_inst_block" $ do

    DSL.compInBlock gthCoreName compInps compOuts

    let inps = zip (fst <$> compInps) args
               <> tiedOffInps
    outs <- mapM (uncurry DSL.declare) compOuts
    DSL.instDecl N.Empty (Id.unsafeMake gthCoreName) gthCoreInstName [] inps (zip (fst <$> compOuts) outs)
    pure [DSL.constructProduct tResult outs]

gthCoreBBTF bbCtx = error ("gthCoreBBTF, bad bbCtx: " <> show bbCtx)

-- | Renders Tcl file conforming to the /Clash\<->Tcl API/, creating the Xilinx
-- IP with @create_ip@
gthCoreTclTF :: HasCallStack => TemplateFunction
gthCoreTclTF =
  TemplateFunction
    [0,1] -- used arguments
    (const True)
    gthCoreTclBBTF

gthCoreTclBBTF ::
  Backend s =>
  BlackBoxContext ->
  State s Doc
gthCoreTclBBTF bbCtx
  | [gthCoreName] <- N.bbQsysIncName bbCtx
  , (exprToString -> Just channelNm,_,_) : (exprToString -> Just refClkNm,_,_) : _ <- N.bbInputs bbCtx
  = pure (renderTcl [IpConfigPurpose $ ipConfig gthCoreName channelNm refClkNm ])
  where
  ipConfig nm channelNm refClkNm = (defIpConfig "gtwizard_ultrascale " "1.7" nm){properties = props channelNm refClkNm}
  props channelNm refClkNm =
    [ property @Text "CHANNEL_ENABLE"                        (fromString channelNm)
    , property @Text "LOCATE_COMMON"                         "CORE"
    , property @Text "LOCATE_IN_SYSTEM_IBERT_CORE"           "NONE"
    , property @Text "LOCATE_RESET_CONTROLLER"               "CORE"
    , property @Text "LOCATE_RX_BUFFER_BYPASS_CONTROLLER"    "CORE"
    , property @Text "LOCATE_RX_USER_CLOCKING"               "CORE"
    , property @Text "LOCATE_TX_BUFFER_BYPASS_CONTROLLER"    "CORE"
    , property @Text "LOCATE_TX_USER_CLOCKING"               "CORE"
    , property @Text "LOCATE_USER_DATA_WIDTH_SIZING"         "CORE"

    , property @Text "FREERUN_FREQUENCY"                     "125.0"

    , property @Text "RX_REFCLK_FREQUENCY"                   "200"
    -- .X_REFCLK_SOURCE syntax: X0Yn clk[0,1]([+,-]q
    , property       "RX_REFCLK_SOURCE"                      (BraceTcl @Text $ fromString $ unwords [channelNm, refClkNm])

    , property @Text "RX_DATA_DECODING"                      "8B10B"
    , property @Text "RX_INT_DATA_WIDTH"                     "40"
    -- , property @Text "RX_JTOL_FC"                            "5.9988002"
    , property @Text "RX_LINE_RATE"                          "10"
    -- , property @Text "RX_MASTER_CHANNEL"                     "X0Y10"
    , property @Text "RX_OUTCLK_SOURCE"                      "RXOUTCLKPMA"

    , property @Text "RX_PLL_TYPE"                           "CPLL"
    , property @Text "RX_PPM_OFFSET"                         "200"
    , property @Text "RX_USER_DATA_WIDTH"                    "64"

    , property @Text "RX_EQ_MODE"                            "LPM"

    , property @Text "RX_COMMA_PRESET" "K28.5"
    , property @Bool "RX_COMMA_P_ENABLE" True
    , property @Bool "RX_COMMA_M_ENABLE" True
    -- , property @Text "RX_COMMA_P_VAL" "0101111100"
    -- , property @Text "RX_COMMA_M_VAL" "1010000011"
    -- , property @Text "RX_COMMA_MASK" "1111111111"
    , property @Bool "RX_COMMA_SHOW_REALIGN_ENABLE" False

    , property @Text "TX_REFCLK_FREQUENCY"                   "200"
    , property       "TX_REFCLK_SOURCE"                      (BraceTcl @Text $ fromString $ unwords [channelNm, refClkNm]) -- "X0Y9 clk0" -- "X0Y10 clk0"

    , property @Text "TXPROGDIV_FREQ_VAL"                    "250"
    , property @Text "TX_DATA_ENCODING"                      "8B10B"
    , property @Text "TX_INT_DATA_WIDTH"                     "40"
    , property @Text "TX_LINE_RATE"                          "10"
    -- , property @Text "TX_MASTER_CHANNEL"                     "X0Y10"
    , property @Text "TX_PLL_TYPE"                           "CPLL"
    , property @Text "TXPROGDIV_FREQ_SOURCE"                 "CPLL"
    , property @Text "TX_USER_DATA_WIDTH"                    "64"

    ]

gthCoreTclBBTF bbCtx = error ("gthCoreTclBBTF, bad bbCtx: " <> show bbCtx)




gthCoreNBBF :: HasCallStack => BlackBoxFunction
gthCoreNBBF _isD _primName _args _resTys
  =
 let
  bbMeta = emptyBlackBoxMeta
    { N.bbKind = N.TDecl
    , N.bbIncludes =
      [ ( ("gth", "clash.tcl")
        , BBFunction (show 'gthCoreNTclTF) 0 gthCoreNTclTF)
      ]
    }

  bb :: BlackBox
  bb = BBFunction (show 'gthCoreNTF) 0 gthCoreNTF
 in
  pure $ Right (bbMeta, bb)

-- | Instantiate IP generated with 'gthCoreNTclTF'
gthCoreNTF :: HasCallStack => TemplateFunction
gthCoreNTF =
  TemplateFunction
    [0..10]
    (const True)
    (gthCoreNBBTF)
 where

gthCoreNBBTF ::
  Backend s =>
  BlackBoxContext ->
  State s Doc
gthCoreNBBTF bbCtx
  | ([chansUsed_,quadsUsed_,_channelNms,_refClkPath,gtrefclk_in,drpclk_in]
    ,args@[
      _gthrxn_in  -- " ::: Signal rxS (BitVector ChansUsed)
    , _gthrxp_in  -- " ::: Signal rxS (BitVector ChansUsed)
    , _gtwiz_reset_clk_freerun_in  -- " ::: Clock freerun
    , _gtwiz_reset_all_in  -- " ::: Reset freerun
    , _gtwiz_reset_tx_pll_and_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_tx_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_rx_pll_and_datapath_in  -- " ::: Reset freerun
    , _gtwiz_reset_rx_datapath_in  -- " ::: Reset freerun
    , _gtwiz_userdata_tx_in  -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH))
    , _txctrl2_in  -- " ::: Signal txUser2 (BitVector (ChansUsed*TX_DATA_WIDTH/8))

    -- , gtrefclk_in  -- " ::: Clock refclk00
    -- , drpclk_in  -- " ::: Clock freerun
    -- , _gtrefclk0_in  -- " ::: Clock refclk0
    ]) <- splitAt 6 $ map fst (DSL.tInputs bbCtx)
  , [tResult] <- map DSL.ety (DSL.tResults bbCtx)
  , [gthCoreNName] <- N.bbQsysIncName bbCtx
  = do
  let
    Just chansUsed = fromInteger <$> DSL.tExprToInteger chansUsed_
    Just quadsUsed = fromInteger <$> DSL.tExprToInteger quadsUsed_

  gthCoreNInstName <- Id.makeBasic "gthcore_inst"
  let
    tX_DATA_WIDTH = 64
    rX_DATA_WIDTH = tX_DATA_WIDTH
    compInps =
      [
        ("gthrxn_in",  N.BitVector chansUsed)
      , ("gthrxp_in",  N.BitVector chansUsed)
      , ("gtwiz_reset_clk_freerun_in",  N.Clock "freerun" )
      , ("gtwiz_reset_all_in",  N.Reset "freerun" )

      , ("gtwiz_reset_tx_pll_and_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_tx_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_rx_pll_and_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_reset_rx_datapath_in",  N.Reset "freerun" )
      , ("gtwiz_userdata_tx_in",  N.BitVector (chansUsed*tX_DATA_WIDTH))
      , ("txctrl2_in",  N.BitVector (chansUsed*(tX_DATA_WIDTH `div` 8)))

      , ("gtrefclk00_in",  N.BitVector quadsUsed) -- N.Clock "refclk00" )
      , ("gtrefclk0_in",  N.BitVector (1*chansUsed)) -- N.Clock "refclk0" )
      , ("drpclk_in",   N.BitVector (1*chansUsed)) -- N.Clock "freerun" )
      ]
    tiedOffInps =
      [
        ("txctrl0_in", DSL.bvLit (16*chansUsed) 0)
      , ("txctrl1_in", DSL.bvLit (16*chansUsed) 0)

      , ("tx8b10ben_in", bvLitAll1 chansUsed)
      , ("rx8b10ben_in", bvLitAll1 chansUsed)

      , ("gtwiz_userclk_tx_reset_in", DSL.bvLit  1 0)
      , ("gtwiz_userclk_rx_reset_in", DSL.bvLit  1 0)

      , ("rxcommadeten_in",    bvLitAll1 chansUsed)
      , ("rxmcommaalignen_in", bvLitAll1 chansUsed)
      , ("rxpcommaalignen_in", bvLitAll1 chansUsed)
      ]
    compOuts =
      [
        ("gthtxn_out",  N.BitVector chansUsed)
      , ("gthtxp_out",  N.BitVector chansUsed)
      , ("gtwiz_userclk_tx_usrclk2_out",  N.Clock "txUser2")
      , ("gtwiz_userclk_rx_usrclk2_out",  N.Clock "rxUser2")
      , ("gtwiz_userdata_rx_out",  N.BitVector (chansUsed*rX_DATA_WIDTH))
      , ("gtwiz_reset_tx_done_out", N.BitVector 1)
      , ("gtwiz_reset_rx_done_out", N.BitVector 1)
      ]

  DSL.declarationReturn bbCtx "gthCoreN_inst_block" $ do

    DSL.compInBlock gthCoreNName compInps (compOuts <> map (fmap DSL.ety) tiedOffInps) -- TODO do we need this?
    gtrefclk0_in <- DSL.vec (replicate chansUsed gtrefclk_in)
    gtrefclk00_in <- DSL.vec (replicate quadsUsed gtrefclk_in)
    drpclks <- DSL.vec (replicate chansUsed drpclk_in)

    let inps = zip (fst <$> compInps) (args <> [gtrefclk00_in,gtrefclk0_in,drpclks])
               <> tiedOffInps
    outs <- mapM (uncurry DSL.declare) compOuts
    DSL.instDecl N.Empty (Id.unsafeMake gthCoreNName) gthCoreNInstName [] inps (zip (fst <$> compOuts) outs)
    pure [DSL.constructProduct tResult outs]

gthCoreNBBTF bbCtx = error ("gthCoreNBBTF, bad bbCtx: " <> show bbCtx)

-- | Construct a BitVector literal with all bits set
bvLitAll1 :: Int -> DSL.TExpr
bvLitAll1 n = DSL.bvLit n (2 ^ n)

-- | Renders Tcl file conforming to the /Clash\<->Tcl API/, creating the Xilinx
-- IP with @create_ip@
gthCoreNTclTF :: HasCallStack => TemplateFunction
gthCoreNTclTF =
  TemplateFunction
    [0,1,2] -- used arguments
    (const True)
    gthCoreNTclBBTF

gthCoreNTclBBTF ::
  Backend s =>
  BlackBoxContext ->
  State s Doc
gthCoreNTclBBTF bbCtx
  | [gthCoreNName] <- N.bbQsysIncName bbCtx
  , (e_chansUsed,_) : _quadsUsed : (e_chanNms,_) : (e_refClkPaths,_) : _ <- DSL.tInputs bbCtx
  = pure (renderTcl [IpConfigPurpose $ ipConfig gthCoreNName (e_chansUsed, e_chanNms, e_refClkPaths) ])
  where
  ipConfig nm chanConfig = (defIpConfig "gtwizard_ultrascale " "1.7" nm){properties = props chanConfig}
  props (e_chansUsed, e_chanNms, e_refClkPaths) =
    [ property       "CHANNEL_ENABLE"                        (BraceTcl @Text $ fromString $ unwords chanNms) -- "X0Y9" -- "X0Y10"

    , property @Text "LOCATE_COMMON"                         "CORE"
    , property @Text "LOCATE_IN_SYSTEM_IBERT_CORE"           "NONE"
    , property @Text "LOCATE_RESET_CONTROLLER"               "CORE"
    , property @Text "LOCATE_RX_BUFFER_BYPASS_CONTROLLER"    "CORE"
    , property @Text "LOCATE_RX_USER_CLOCKING"               "CORE"
    , property @Text "LOCATE_TX_BUFFER_BYPASS_CONTROLLER"    "CORE"
    , property @Text "LOCATE_TX_USER_CLOCKING"               "CORE"
    , property @Text "LOCATE_USER_DATA_WIDTH_SIZING"         "CORE"

    -- , property @Text "PRESET"                                "GTH-Gigabit_Ethernet"

    , property @Text "FREERUN_FREQUENCY"                     "125"

    , property @Text "RX_REFCLK_FREQUENCY"                   "200"
    -- .X_REFCLK_SOURCE syntax: X0Yn clk[0,1]([+,-]q
    , property       "RX_REFCLK_SOURCE"                      refClkProp

    , property @Text "RX_DATA_DECODING"                      "8B10B"
    , property @Text "RX_INT_DATA_WIDTH"                     "40"
    -- , property @Text "RX_JTOL_FC"                            "5.9988002"
    , property @Text "RX_LINE_RATE"                          "10"
    -- , property @Text "RX_MASTER_CHANNEL"                     "X0Y10"
    , property @Text "RX_OUTCLK_SOURCE"                      "RXOUTCLKPMA"
    , property @Text "RX_PLL_TYPE"                           "CPLL"
    , property @Text "RX_PPM_OFFSET"                         "200"
    , property @Text "RX_USER_DATA_WIDTH"                    "64"

    -- , property @Text "INS_LOSS_NYQ"                          "14"
    , property @Text "RX_EQ_MODE"                            "LPM"

    , property @Text "RX_COMMA_PRESET" "K28.5"
    , property @Bool "RX_COMMA_P_ENABLE" True
    , property @Bool "RX_COMMA_M_ENABLE" True
    -- , property @Text "RX_COMMA_P_VAL" "0101111100"
    -- , property @Text "RX_COMMA_M_VAL" "1010000011"
    -- , property @Text "RX_COMMA_MASK" "1111111111"
    , property @Bool "RX_COMMA_SHOW_REALIGN_ENABLE" False

    , property @Text "TX_REFCLK_FREQUENCY"                   "200"
    , property       "TX_REFCLK_SOURCE"                      refClkProp

    , property @Text "TXPROGDIV_FREQ_VAL"                    "250"
    , property @Text "TX_DATA_ENCODING"                      "8B10B"
    , property @Text "TX_INT_DATA_WIDTH"                     "40"
    , property @Text "TX_LINE_RATE"                          "10"
    -- , property @Text "TX_MASTER_CHANNEL"                     "X0Y10"
    , property @Text "TX_PLL_TYPE"                           "QPLL0"
    , property @Text "TX_USER_DATA_WIDTH"                    "64"
    ]
   where
    chansUsed :: Int
    chansUsed = case fromInteger <$> DSL.tExprToInteger e_chansUsed of
      Just x -> x
      Nothing -> error $ "Can't get integer from " <> show e_chansUsed
    chanNms :: [String]
    chanNms = case mapM DSL.getStr =<< DSL.getVec e_chanNms of
      Just x | chansUsed /= length x -> error $ "chansUsed (" <> show chansUsed <> "), doesn't match chanNms " <> show x
             | otherwise -> x
      Nothing -> error $ "Can't get string from " <> show e_chanNms
    refClkPaths :: [String]
    refClkPaths = case mapM DSL.getStr =<< DSL.getVec e_refClkPaths of
      Just x | chansUsed /= length x -> error $ "chansUsed (" <> show chansUsed <> "), doesn't match refClkPaths " <> show x
             | otherwise -> x
      Nothing -> error $ "Can't get string from " <> show e_refClkPaths

    refClkProp = BraceTcl @Text $ fromString $ unwords $ zipWith (\x y -> x <> " " <> y) chanNms refClkPaths

gthCoreNTclBBTF bbCtx = error ("gthCoreNTclBBTF, bad bbCtx: " <> show bbCtx)




ibufds_gte3BBF :: HasCallStack => BlackBoxFunction
ibufds_gte3BBF _isD _primName _args _resTys
  =
 let
  bbMeta = emptyBlackBoxMeta { N.bbKind = N.TDecl }

  bb :: BlackBox
  bb = BBFunction (show 'ibufds_gte3TF) 0 ibufds_gte3TF
 in
  pure $ Right (bbMeta, bb)

ibufds_gte3TF :: HasCallStack => TemplateFunction
ibufds_gte3TF =
  TemplateFunction
    [0,1]
    (const True)
    ibufds_gte3BBTF

ibufds_gte3BBTF ::
  Backend s =>
  BlackBoxContext ->
  State s Doc
ibufds_gte3BBTF bbCtx
  | args@[
      _clk_n
    , _clk_p
    ] <- map fst (DSL.tInputs bbCtx)
  = do

  ibufds_gte3InstName <- Id.makeBasic "ibufds_gte3_inst"

  let
    compInps =
      [
        ("IB",  N.Clock "dom")
      , ("I", N.Clock "dom")
      ]
    tiedOffInps =
      [
        ("CEB", DSL.Low)
      ]
    compOuts =
      [
        ("O",  N.Clock "dom")
      ]
    attrs =
      [
        ("REFCLK_EN_TX_PATH",  DSL.Low)
      , ("REFCLK_HROW_CK_SEL", DSL.bvLit 2 0b10)
      , ("REFCLK_ICNTL_RX",    DSL.bvLit 2 0b00)
      ]
    ibufds_gte3Name = "IBUFDS_GTE3"
  DSL.declarationReturn bbCtx "ibufds_gte3_inst_block" $ do

    let inps = zip (fst <$> compInps) args
               <> tiedOffInps
    outs <- mapM (uncurry DSL.declare) compOuts
    DSL.instDecl N.Empty (Id.unsafeMake ibufds_gte3Name) ibufds_gte3InstName attrs inps (zip (fst <$> compOuts) outs)
    pure outs

ibufds_gte3BBTF bbCtx = error ("ibufds_gte3BBTF, bad bbCtx: " <> show bbCtx)
