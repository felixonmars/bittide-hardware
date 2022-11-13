-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Bittide.Simulate.Time where

import Prelude

import Clash.Signal.Internal (Femtoseconds (Femtoseconds), mapFemtoseconds)
import Data.Int (Int64)

seconds :: Int64 -> Femtoseconds
seconds s = mapFemtoseconds (* 1000) (milliseconds s)

milliseconds :: Int64 -> Femtoseconds
milliseconds s = mapFemtoseconds (* 1000) (microseconds s)

microseconds :: Int64 -> Femtoseconds
microseconds s = mapFemtoseconds (* 1000) (nanoseconds s)

nanoseconds :: Int64 -> Femtoseconds
nanoseconds s = mapFemtoseconds (* 1000) (picoseconds s)

picoseconds :: Int64 -> Femtoseconds
picoseconds s = mapFemtoseconds (* 1000) (femtoseconds s)

femtoseconds :: Int64 -> Femtoseconds
femtoseconds = Femtoseconds
