module Data.Bool.Extra where

import Prelude

-- $setup
-- >>> import Prelude

-- | Logical implication
--
-- >>> True `implies` True
-- True
-- >>> False `implies` True
-- True
-- >>> False `implies` False
-- True
-- >>> True `implies` False
-- False
--
implies :: Bool -> Bool -> Bool
implies = (<=)
infixr 1 `implies`
