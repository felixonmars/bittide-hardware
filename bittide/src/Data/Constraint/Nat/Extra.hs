{-|
Copyright:           Copyright © 2022, Google LLC
License:             Apache-2.0
Maintainer:          devops@qbaylogic.com
|-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Constraint.Nat.Extra where

import Data.Constraint
import GHC.TypeNats
import Unsafe.Coerce

-- | b <= ceiling(b/a)*a
timesDivRU :: forall a b . Dict (b <= ((Div (b + (a - 1)) a) * a))
timesDivRU = unsafeCoerce (Dict :: Dict ())
