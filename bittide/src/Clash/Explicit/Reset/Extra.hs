module Clash.Explicit.Reset.Extra where

import Clash.Explicit.Prelude

import Clash.Cores.Extra

safeResetSynchronizer ::
  (KnownDomain src, KnownDomain dst) =>
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
safeResetSynchronizer clkSrc clkDest rstSrc = unsafeFromHighPolarity
  $ safeDffSynchronizer clkSrc clkDest True (unsafeToHighPolarity rstSrc)

orReset :: KnownDomain dom => Reset dom -> Reset dom -> Reset dom
orReset a b = unsafeFromHighPolarity (unsafeToHighPolarity a .||. unsafeToHighPolarity b)

noReset :: KnownDomain dom => Reset dom
noReset = unsafeFromHighPolarity (pure False)
