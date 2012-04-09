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
delayS :: a -> Signal a -> SignalGen (Signal a)
delayS = delay

-- | Same as 'FRP.Elerea.Simple.generator'
generatorS :: Signal (SignalGen a) -> SignalGen (Signal a)
generatorS = generator

-- | Same as 'FRP.Elerea.Simple.snapshot'
snapshotS :: Signal a -> SignalGen a
snapshotS = snapshot

-- | Same as 'FRP.Elerea.Simple.memo'
memoS :: Signal a -> SignalGen (Signal a)
memoS = memo
