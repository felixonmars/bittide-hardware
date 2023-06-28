-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude

import Control.Monad.Extra (ifM, unlessM, when)
import Data.Foldable (for_)
import Development.Shake
import GHC.Stack (HasCallStack)
import Language.Haskell.TH (nameBase)
import System.Console.ANSI (setSGR)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath (isDrive, (</>), takeDirectory)

import Paths_bittide_instances

import Clash.Shake.Flags
import Clash.Shake.Vivado
import Clash.Shake.Extra
import Development.Shake.Extra

import qualified Bittide.Instances.BoardTest as BoardTest
import qualified Bittide.Instances.Calendar as Calendar
import qualified Bittide.Instances.ClockControl as ClockControl
import qualified Bittide.Instances.Counter as Counter
import qualified Bittide.Instances.ElasticBuffer as ElasticBuffer
import qualified Bittide.Instances.MVPs as MVPs
import qualified Bittide.Instances.ScatterGather as ScatterGather
import qualified Bittide.Instances.Si539xSpi as Si539xSpi
import qualified Bittide.Instances.StabilityChecker as StabilityChecker
import qualified Bittide.Instances.Synchronizer as Synchronizer
import qualified Clash.Util.Interpolate as I
import qualified Language.Haskell.TH as TH
import qualified System.Directory as Directory

-- | Given Cabal project root, determine build directory
buildDir :: FilePath
buildDir = "_build"

-- | Given Cabal project root, determine Clash HDL output directory
clashBuildDir :: FilePath
clashBuildDir = buildDir </> "clash"

-- | Given Cabal project root, determine directory for Vivado input + output files
vivadoBuildDir :: FilePath
vivadoBuildDir = buildDir </> "vivado"

getConstraintFilePath :: TH.Name -> IO FilePath
getConstraintFilePath target =
  getDataFileName ("data" </> "constraints" </> nameBase target <> ".xdc")


-- | Searches for a file called @cabal.project@ It will look for it in the
-- current working directory. If it can't find it there, it will traverse up
-- until it finds the file.
--
-- The returned path points to the directory containing @cabal.project@. Errors
-- if it could not find @cabal.project@ anywhere.
--
findProjectRoot :: HasCallStack => IO FilePath
findProjectRoot = goUp =<< getCurrentDirectory
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error "Could not find 'cabal.project'"
    | otherwise =
        ifM
          (Directory.doesFileExist (path </> projectFilename))
          (return path)
          (goUp (takeDirectory path))

  projectFilename = "cabal.project"

data Target = Target
  { -- | TemplateHaskell reference to top entity to synthesize
    targetName :: TH.Name

    -- | Whether target has an associated XDC file in 'data/constraints'. An XDC
    -- file implies that a bitstream can be generated.
  , targetHasXdc :: Bool

    -- | Whether target has one or more VIOs
  , targetHasVio :: Bool

    -- | Whether target has a VIO probe that can be used to run hardware-in-the-
    -- loop tests. Note that this flag, 'targetHasTest', implies 'targetHasVio'.
  , targetHasTest :: Bool
  }

defTarget :: TH.Name -> Target
defTarget name = Target
  { targetName = name
  , targetHasXdc = False
  , targetHasVio = False
  , targetHasTest = False
  }

enforceValidTarget :: Target -> Target
enforceValidTarget target@Target{..}
  | targetHasTest && not targetHasVio =
      error $ show targetName <> " should have set 'targetHasVio', because " <>
                                 "'targetHasTest' was asserted."
  | otherwise = target


-- | All synthesizable targets
targets :: [Target]
targets = map enforceValidTarget
  [ (defTarget 'BoardTest.simpleHardwareInTheLoopTest)
    { targetHasXdc = True
    , targetHasVio = True
    , targetHasTest = True
    }
  , defTarget 'Calendar.switchCalendar1k
  , defTarget 'Calendar.switchCalendar1kReducedPins
  , defTarget 'ClockControl.callisto3
  , defTarget 'Counter.counterReducedPins
  , defTarget 'ElasticBuffer.elasticBuffer5
  , (defTarget 'MVPs.clockControlDemo0) {targetHasXdc = True}
  , (defTarget 'MVPs.clockControlDemo1) {targetHasXdc = True}
  , defTarget 'ScatterGather.gatherUnit1K
  , defTarget 'ScatterGather.gatherUnit1KReducedPins
  , defTarget 'ScatterGather.scatterUnit1K
  , defTarget 'ScatterGather.scatterUnit1KReducedPins
  , defTarget 'Si539xSpi.callistoSpi
  , defTarget 'Si539xSpi.si5391Spi
  , defTarget 'StabilityChecker.stabilityChecker_3_1M
  , defTarget 'Synchronizer.safeDffSynchronizer
  ]

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions
  { shakeFiles = buildDir
  , shakeChange = ChangeDigest
  , shakeVersion = "5"
  }

-- | Run Vivado on given TCL script
vivadoFromTcl :: FilePath -> Action ()
vivadoFromTcl tclPath =
  command_
    [AddEnv "XILINX_LOCAL_USER_DATA" "no"] -- Prevents multiprocessing issues
    "vivado"
    ["-mode", "batch", "-source", tclPath]

-- | Defines a Shake build executable for calling Vivado. Like Make, in Shake
-- you define rules that explain how to build a certain file. For example:
--
--     manifestPath %> ...
--
-- means: to build @manifestPath@ I need to do dot-dot-dot. See the README for
-- an overview of which commands are user-passable (or simply scroll down).
--
-- For a fundamental introduction into Shake, read the (lightweight!) paper
-- introducing it:
--
--   https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf.
--
-- Or, see https://shakebuild.com/.
--
main :: IO ()
main = do
  setCurrentDirectory =<< findProjectRoot

  shakeArgsWith shakeOpts customFlags $ \flags shakeTargets -> pure $ Just $ do

    let
      hwTargets = getHardwareTargetsFlag flags

      rules = do
        -- 'all' builds all targets defined below
        phony "all" $ do
          for_ targets $ \Target{..} -> do
            need [nameBase targetName <> ":synth"]

        -- For each target, generate a user callable command (PHONY). Run with
        -- '--help' to list them.
        for_ targets $ \Target{..}-> do
          let
            -- TODO: Dehardcode these paths. They're currently hardcoded in both the
            --       TCL and here, which smells.
            manifestPath = getManifestLocation clashBuildDir targetName
            synthesisDir = vivadoBuildDir </> show targetName
            checkpointsDir = synthesisDir </> "checkpoints"
            netlistDir = synthesisDir </> "netlist"
            reportDir = synthesisDir </> "reports"

            runSynthTclPath        = synthesisDir </> "run_synth.tcl"
            runPlaceTclPath        = synthesisDir </> "run_place.tcl"
            runRouteTclPath        = synthesisDir </> "run_route.tcl"
            runNetlistTclPath      = synthesisDir </> "run_netlist.tcl"
            runBitstreamTclPath    = synthesisDir </> "run_bitstream.tcl"
            runProbesGenTclPath    = synthesisDir </> "run_probes_gen.tcl"
            runBoardProgramTclPath = synthesisDir </> "run_board_program.tcl"
            runHardwareTestTclPath = synthesisDir </> "run_hardware_test.tcl"

            postSynthCheckpointPath     = checkpointsDir </> "post_synth.dcp"
            postPlaceCheckpointPath     = checkpointsDir </> "post_place.dcp"
            postRouteCheckpointPath     = checkpointsDir </> "post_route.dcp"
            postNetlistCheckpointPath   = checkpointsDir </> "post_netlist.dcp"

            netlistPaths =
              [ netlistDir </> "netlist.v"
              , netlistDir </> "netlist.xdc"
              ]
            bitstreamPath = synthesisDir </> "bitstream.bit"
            probesPath = synthesisDir </> "probes.ltx"

            postRouteTimingSummaryPath = reportDir </> "post_route_timing_summary.rpt"
            postRouteTimingPath = reportDir </> "post_route_timing.rpt"

            synthReportsPaths = [reportDir </> "post_synth_timing_summary.rpt"]
            placeReportPaths = [reportDir </> "post_place_timing_summary.rpt"]
            routeReportsPaths =
              [ reportDir </> "post_route_clock_util.rpt"
              , reportDir </> "post_route_drc.rpt"
              , reportDir </> "post_route_power.rpt"
              , reportDir </> "post_route_timing.rpt"
              , reportDir </> "post_route_timing_summary.rpt"
              , reportDir </> "post_route_util.rpt"
              ]

          withoutTargets $ do
            manifestPath %> \path -> do
              needDirectory "dist-newstyle"
              let
                (buildTool, buildToolArgs) =
                  defaultClashCmd clashBuildDir targetName
              command_ [] buildTool buildToolArgs

              -- Clash messes up ANSI escape codes, leaving the rest of the terminal
              -- printed in bold text. Reset manually:
              liftIO (setSGR [])

              produces [path]

            -- Synthesis
            runSynthTclPath %> \path -> do
              constraintFilePath <- liftIO (getConstraintFilePath targetName)

              constraints <-
                if targetHasXdc then do
                  need [constraintFilePath]
                  pure [constraintFilePath]
                else
                  pure []

              synthesisPart <- getEnvWithDefault "xcku035-ffva1156-2-e" "SYNTHESIS_PART"
              locatedManifest <- decodeLocatedManifest manifestPath

              tcl <- liftIO $
                mkSynthesisTcl
                  synthesisDir            -- Output directory for Vivado
                  False                   -- Out of context run
                  synthesisPart           -- Part we're synthesizing for
                  constraints             -- List of filenames with constraints
                  locatedManifest

              writeFileChanged path tcl

            (postSynthCheckpointPath : synthReportsPaths) |%> \_ -> do
              need [runSynthTclPath, manifestPath]
              vivadoFromTcl runSynthTclPath

            -- Placement
            runPlaceTclPath %> \path -> do
              writeFileChanged path (mkPlaceTcl synthesisDir)

            (postPlaceCheckpointPath : placeReportPaths) |%> \_ -> do
              need [runPlaceTclPath, postSynthCheckpointPath]
              vivadoFromTcl runPlaceTclPath

            -- Routing
            runRouteTclPath %> \path -> do
              writeFileChanged path (mkRouteTcl synthesisDir)

            (postRouteCheckpointPath : routeReportsPaths) |%> \_ -> do
              need [runRouteTclPath, postPlaceCheckpointPath]
              vivadoFromTcl runRouteTclPath

              -- Design should meet timing post routing. Note that this is not a
              -- requirement after synthesis as many of the optimizations only follow
              -- after.
              liftIO $ unlessM
                (meetsTiming postRouteTimingSummaryPath)
                (error [I.i|
                  Design did not meet timing. Check out the timing summary at:

                    #{postRouteTimingSummaryPath}

                  Alternatively, check out the full report:

                    #{postRouteTimingPath}

                  You can investigate interactively by opening the latest checkpoint with Vivado:

                    vivado #{postRouteCheckpointPath}

                |])

            -- Netlist generation
            runNetlistTclPath %> \path -> do
              writeFileChanged path (mkNetlistTcl synthesisDir)

            (postNetlistCheckpointPath : netlistPaths) |%> \_ -> do
              need [runNetlistTclPath, postRouteCheckpointPath]
              vivadoFromTcl runNetlistTclPath

            -- Bitstream generation
            runBitstreamTclPath %> \path -> do
              writeFileChanged path (mkBitstreamTcl synthesisDir)

            bitstreamPath %> \_ -> do
              need [runBitstreamTclPath, postNetlistCheckpointPath]
              vivadoFromTcl runBitstreamTclPath

            -- Probes file generation
            runProbesGenTclPath %> \path -> do
              writeFileChanged path (mkProbesGenTcl synthesisDir)

            probesPath %> \_ -> do
              need [runProbesGenTclPath, bitstreamPath]
              vivadoFromTcl runProbesGenTclPath

            -- Write bitstream to board
            runBoardProgramTclPath %> \path -> do
              alwaysRerun
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              boardProgramTcl <-
                liftIO $ mkBoardProgramTcl synthesisDir hwTargets url targetHasVio
              writeFileChanged path boardProgramTcl

            -- Run hardware test
            runHardwareTestTclPath %> \path -> do
              alwaysRerun
              url <- getEnvWithDefault "localhost:3121" "HW_SERVER_URL"
              hardwareTestTcl <- liftIO $ mkHardwareTestTcl synthesisDir hwTargets url
              writeFileChanged path hardwareTestTcl


          -- User friendly target names
          phony (nameBase targetName <> ":hdl") $ do
            need [manifestPath]

          phony (nameBase targetName <> ":synth") $ do
            need [postSynthCheckpointPath]

          phony (nameBase targetName <> ":place") $ do
            need [postPlaceCheckpointPath]

          phony (nameBase targetName <> ":route") $ do
            need [postRouteCheckpointPath]

          phony (nameBase targetName <> ":netlist") $ do
            need [postNetlistCheckpointPath]

          when targetHasXdc $ do
            phony (nameBase targetName <> ":bitstream") $ do
              when targetHasVio $ need [probesPath]
              need [bitstreamPath]

            phony (nameBase targetName <> ":program") $ do
              when targetHasVio $ need [probesPath]
              need [runBoardProgramTclPath, bitstreamPath]
              vivadoFromTcl runBoardProgramTclPath

            when targetHasTest $ do
              phony (nameBase targetName <> ":test") $ do
                need
                  [ runBoardProgramTclPath
                  , runHardwareTestTclPath
                  , bitstreamPath
                  , probesPath
                  ]
                vivadoFromTcl runBoardProgramTclPath
                vivadoFromTcl runHardwareTestTclPath

    if null shakeTargets then
      rules
    else
      want shakeTargets >> withoutActions rules
