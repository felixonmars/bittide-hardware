-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: BSD-2-Clause
module Clash.Explicit.Signal.Extra (glitchFilter) where
import Clash.Explicit.Prelude

data GlitchFilterState a n = GlitchFilterState { currentState :: a, previousInput :: a, stableCounter :: Index n } deriving (Generic, NFDataX)

-- |

glitchFilter
  :: forall dom glitchlessPeriod a {n}
   . ( KnownDomain dom
     , glitchlessPeriod ~ (n + 2)
     , NFDataX a
     , Eq a)
  => SNat glitchlessPeriod
  -> Clock dom
  -> Reset dom
  -> a
  -> Signal dom a
  -> Signal dom a
glitchFilter SNat clk rst initVal a =
  moore clk rst enableGen go currentState (GlitchFilterState initVal initVal 0) a

 where
  go :: state ~ (GlitchFilterState a (glitchlessPeriod - 1)) => state -> a -> state
  go state inp
    | inp /= previousInput state      = state { previousInput = inp, stableCounter = 0 }
    | stableCounter state == maxBound = state { currentState = previousInput state }
    | otherwise                       = state { stableCounter = stableCounter state + 1}

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{- # CLASH_OPAQUE glitchFilter #-} -- Give reset glitch filter its own HDL file
