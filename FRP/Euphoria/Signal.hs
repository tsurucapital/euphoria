{-# OPTIONS_GHC -Wall #-}
-- | Re-exported and renamed definitions from FRP.Elerea.Simple.
module FRP.Euphoria.Signal
    (
    -- * Re-exports
      Signal
    , SignalGen
    , execute
    , external
    , start

    -- * Renamed functions
    , delayS
    , generatorS
    , snapshotS
    , memoS
    ) where

import FRP.Elerea.Simple

-- | Same as 'FRP.Elerea.Simple.delay'
--
-- @delayS sig@ returns a 'Signal' whose value is equal to
-- the value of @sig@ in the previous step. This function
-- does not introduce a direct dependency; for example it
-- is ok if @sig@ depends on the resulting signal of the
-- call.
delayS :: a -> Signal a -> SignalGen (Signal a)
delayS = delay

-- | Same as 'FRP.Elerea.Simple.generator'
--
-- @generatorS net@ runs the 'SignalGen' action specified
-- by @net@ each step. @generatorS@ returns a signal that
-- contains the value returned by the action in this step.
generatorS :: Signal (SignalGen a) -> SignalGen (Signal a)
generatorS = generator

-- | Same as 'FRP.Elerea.Simple.snapshot'
--
-- @snapshotS sig@ returns the current value of @sig@.
snapshotS :: Signal a -> SignalGen a
snapshotS = snapshot

-- | Same as 'FRP.Elerea.Simple.memo'
--
-- @memoS sig@ returns a memoized version of @sig@. The returned
-- signal can be used any number of times without the risk of
-- duplicated computation.
memoS :: Signal a -> SignalGen (Signal a)
memoS = memo
