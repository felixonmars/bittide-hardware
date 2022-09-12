-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Main (main) where

import Control.Monad (when)
import System.Console.Docopt
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Bittide.Topology

readOrError :: String -> Int
readOrError s =
  case readMaybe s of
    Nothing -> error ("Could not parse '" <> s <> "' as 'Int'")
    Just a  -> a

patterns :: Docopt
patterns = [docopt|
sim version 0.1.0

Usage:
  sim csv <steps> <sample>
  sim plot <steps> <sample>

Options:
  <sample> Plot/dump only every kth datum
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "csv")) $ do
    n <- args `getArgOrExit` (argument "steps")
    k <- args `getArgOrExit` (argument "sample")
    dumpCsv (readOrError n) (readOrError k)

  when (args `isPresent` (command "plot")) $ do
    n <- args `getArgOrExit` (argument "steps")
    k <- args `getArgOrExit` (argument "sample")
    plotEbs (readOrError n) (readOrError k)
