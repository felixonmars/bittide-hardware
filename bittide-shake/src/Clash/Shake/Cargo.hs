module Clash.Shake.Cargo where

import Prelude

import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

cargoBuildFirmwareProgram ::
  FilePath ->
  -- ^ Directory to perform the call in
  FilePath ->
  -- ^ Output directory
  String ->
  -- ^ name of the binary to build
  Bool ->
  -- ^ release mode?
  IO (Maybe FilePath)
cargoBuildFirmwareProgram workingDir outDir name release = do
  oldEnv <- getEnvironment
  let
    newEnv = ("CARGO_TARGET_DIR", outDir) : oldEnv
    createProc =
      (proc "cargo" (["build", "--bin", name] <> ["--release" | release]))
        { cwd = Just workingDir, env = Just newEnv }
  (exitCode, _stdoutOut, stderrOut) <- readCreateProcessWithExitCode createProc ""

  case exitCode of
    ExitSuccess -> do
      pure $ Just (rustBinaryPath outDir name release)
    ExitFailure _ -> do
      hPutStrLn stderr "Compilation failed"
      hPutStrLn stderr stderrOut
      pure Nothing

rustBinaryPath :: FilePath -> String -> Bool -> FilePath
rustBinaryPath outDir name release = outDir
  </> "riscv32imc-unknown-none-elf"
  </> (if release then "release" else "debug")
  </> name
