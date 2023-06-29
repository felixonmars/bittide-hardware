-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Generate a TCL script to simulate generated VHDL
--
-- Run with @vivado -mode batch -source ...@
--
module Clash.Shake.Vivado
  ( LocatedManifest(..)
  , decodeLocatedManifest
  , mkPlaceTcl
  , mkNetlistTcl
  , mkSynthesisTcl
  , mkRouteTcl
  , mkBitstreamTcl
  , mkProbesGenTcl
  , mkBoardProgramTcl
  , mkHardwareTestTcl
  , meetsTiming
  ) where

import Prelude

import Development.Shake
import Development.Shake.Extra (decodeFile)

import Clash.DataFiles (tclConnector)
import Clash.Driver.Manifest
import Data.List (isInfixOf)
import Data.String.Interpolate (__i)
import System.FilePath ((</>), dropFileName)

import Clash.Shake.Extra (hexDigestFile)
import Clash.Shake.Flags (HardwareTargets(..))

import Paths_bittide_instances

-- | Read a timing summary and determine whether it met timing.
meetsTiming :: FilePath -> IO Bool
meetsTiming reportPath = do
  reportContents <- readFile reportPath
  pure $ not ("Timing constraints are not met." `isInfixOf` reportContents)

-- TODO: Upstream
data LocatedManifest = LocatedManifest
  { -- | Path pointing to the manifest file itself
    lmPath :: FilePath

    -- | Manifest file corresponding to the one at 'lmPath'
  , lmManifest :: Manifest
  }

decodeLocatedManifest :: FilePath -> Action LocatedManifest
decodeLocatedManifest path = LocatedManifest path <$> decodeFile path

-- | Generates TCL that generates and reads Xilinx IP and reads constraints and
-- HDL files generated by Clash. The caller is responsible for starting synthesis
-- or simulation.
--
mkBaseTcl ::
  -- | Where to create ip directory.
  FilePath ->
  -- | Top entity directory
  LocatedManifest ->
  -- | TCL script
  Action String
mkBaseTcl outputDir LocatedManifest{lmPath} = do
  connector <- liftIO tclConnector
  connectorDigest <- hexDigestFile connector
  lmPathDigest <- hexDigestFile lmPath
  let topEntityDir = dropFileName lmPath

  pure [__i|
    \# #{lmPath}: #{lmPathDigest}
    \# #{connector}: #{connectorDigest}
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR
    source -notrace {#{connector}}
    file delete -force {#{outputDir </> "ip"}}
    file mkdir {#{outputDir </> "ip"}}
    clash::readMetadata {#{topEntityDir}}
    clash::createAndReadIp -dir {#{outputDir </> "ip"}}
    clash::readHdl
    clash::readXdc {early normal late}
    set_property TOP $clash::topEntity [current_fileset]
  |]

mkSynthesisTcl ::
  -- | Directory to write logs and checkpoints to
  FilePath ->
  -- | Out of context?
  Bool ->
  -- | Part to synthesize for. E.g., 'xcku040-ffva1156-2-e'.
  String ->
  -- | List of filepaths to XDC files
  [FilePath] ->
  -- | Manifests of which the first is the top-level to synthesize
  LocatedManifest ->
  -- | Rendered TCL
  Action String
mkSynthesisTcl outputDir outOfContext part constraints manifest@LocatedManifest{lmManifest} = do
  baseTcl <- mkBaseTcl outputDir manifest
  constraintDigests <- unlines <$> mapM constraintDigest constraints
  pure $ baseTcl <> "\n" <> [__i|
    #{constraintDigests}
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    #{constraintsString}
    file mkdir {#{outputDir </> "reports"}}
    file mkdir {#{outputDir </> "checkpoints"}}

    \# Synthesis
    synth_design -name #{name} -part #{part} -mode #{outOfContextStr}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_synth_timing_summary.rpt"}}
    report_utilization -file {#{outputDir </> "reports" </> "post_synth_util.rpt"}}
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_synth.dcp"}}

    \# Netlist
    file mkdir {#{outputDir </> "netlist"}}
    write_verilog -force {#{outputDir </> "netlist" </> "netlist.v"}}
    write_xdc -no_fixed_only -force {#{outputDir </> "netlist" </> "netlist.xdc"}}
  |]
 where
  name = topComponent lmManifest
  outOfContextStr
    | outOfContext = "out_of_context" :: String
    | otherwise    = "default"
  constraintReader constr = "read_xdc {" <> constr <> "}\n"
  constraintsString = concatMap constraintReader constraints

  constraintDigest path = do
    pathDigest <- hexDigestFile path
    pure [__i|\# #{path}: #{pathDigest}|]

mkPlaceTcl :: FilePath -> String
mkPlaceTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where synthesis left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_synth.dcp"}}

    \# Place all clocks in individual clock groups and make them asynchronous
    set clkArgs {}
    foreach clk [get_clocks] {
      lappend clkArgs -group $clk
    }
    set_clock_groups -asynchronous {*}$clkArgs

    \# Run optimization & placement
    opt_design
    place_design
    phys_opt_design
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_place.dcp"}}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_place_timing_summary.rpt"}}
|]

mkRouteTcl :: FilePath -> String
mkRouteTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where placement left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_place.dcp"}}

    \# Routing
    route_design
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_route.dcp"}}
    report_timing_summary -file {#{outputDir </> "reports" </> "post_route_timing_summary.rpt"}}
    report_timing -sort_by group -max_paths 100 -path_type summary -file {#{outputDir </> "reports" </> "post_route_timing.rpt"}}

    report_clock_utilization -file {#{outputDir </> "reports" </> "post_route_clock_util.rpt"}}
    report_utilization       -file {#{outputDir </> "reports" </> "post_route_util.rpt"}}
    report_power             -file {#{outputDir </> "reports" </> "post_route_power.rpt"}}
    report_drc               -file {#{outputDir </> "reports" </> "post_route_drc.rpt"}}
|]

mkNetlistTcl :: FilePath -> String
mkNetlistTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where routing left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_route.dcp"}}

    \# Generate netlist and constraints
    file mkdir {#{outputDir </> "netlist"}}
    write_verilog -force {#{outputDir </> "netlist" </> "netlist.v"}}
    write_xdc -no_fixed_only -force {#{outputDir </> "netlist" </> "netlist.xdc"}}
    write_checkpoint -force {#{outputDir </> "checkpoints" </> "post_netlist.dcp"}}
|]

mkBitstreamTcl :: FilePath -> String
mkBitstreamTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where netlist left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_netlist.dcp"}}

    \# Generate bitstream
    write_bitstream -force {#{outputDir </> "bitstream.bit"}}
|]

mkProbesGenTcl :: FilePath -> String
mkProbesGenTcl outputDir = [__i|
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    \# Pick up where netlist left off
    open_checkpoint {#{outputDir </> "checkpoints" </> "post_netlist.dcp"}}

    \# Generate probes file
    write_debug_probes -force {#{outputDir </> "probes.ltx"}}
|]

-- | Convert HardwareTargets to a Tcl list of target FPGAs. To be used in
-- combination with `fpga_ids` in `HardwareTest.tcl`
toTclTarget :: HardwareTargets -> String
toTclTarget hwTargets = case hwTargets of
  FirstOfAny -> "[list -1]"
  FirstOfKnown -> "[list 0]"
  All -> "{0 1 2 3 4 5 6 7}"

mkBoardProgramTcl ::
  -- | Directory where the bitstream file are located
  FilePath ->
  -- | Hardware targets to program, see `Flags.hs`
  HardwareTargets ->
  -- | Hardware server URL
  String ->
  -- | Flag indicating if the target has a probes file. If true, the probes file
  -- is programmed alongside the bitstream.
  Bool ->
  -- | Rendered Tcl
  IO String
mkBoardProgramTcl outputDir hwTargets url hasProbesFile = do
  hardwareTestTclPath <- getDataFileName ("data" </> "tcl" </> "HardwareTest.tcl")
  let
    probesTcl :: String
    probesTcl
      | hasProbesFile = [__i|set probes_file {#{outputDir </> "probes.ltx"}}|]
      | otherwise = "set probes_file {}"

  pure [__i|
    source {#{hardwareTestTclPath}}
    global fpga_ids

    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    set fpga_nrs #{toTclTarget hwTargets}
    set program_file {#{outputDir </> "bitstream.bit"}}
    set url {#{url}}
    #{probesTcl}

    set expected_targets [llength $fpga_nrs]
    connect_expected_targets ${url} ${expected_targets}

    if {[lindex $fpga_nrs 0] == -1} {
      set device [load_first_device]
      program_fpga ${program_file} ${probes_file}
    } else {
      foreach fpga_nr $fpga_nrs {
        set target_id [lindex $fpga_ids $fpga_nr]
        set target_name [get_part_name $url $target_id]
        set device [load_target_device $target_name]
        program_fpga ${program_file} ${probes_file}
      }
    }
  |]


mkHardwareTestTcl ::
  -- | Directory where the probes file is located
  FilePath ->
  -- | Hardware targets to test, see `Flags.hs`
  HardwareTargets ->
  -- | Hardware server URL
  String ->
  -- | Rendered Tcl
  IO String
mkHardwareTestTcl outputDir hwTargets url = do
  hardwareTestTclPath <- getDataFileName ("data" </> "tcl" </> "HardwareTest.tcl")
  pure [__i|
    source {#{hardwareTestTclPath}}
    set_msg_config -severity {CRITICAL WARNING} -new_severity ERROR

    set fpga_nrs #{toTclTarget hwTargets}
    set probes_file {#{outputDir </> "probes.ltx"}}
    set url {#{url}}

    set expected_targets [llength $fpga_nrs]
    connect_expected_targets ${url} ${expected_targets}

    run_test_all $probes_file $fpga_nrs $url
  |]
