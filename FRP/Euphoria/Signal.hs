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
    -- * MonadSignalGen
    , MonadSignalGen(..)
    -- * Renamed functions
    , delayS
    , generatorS
    , snapshotS
    , memoS
    , transferS
    ) where

import FRP.Elerea.Simple
import Control.Monad.Fix
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Except

import Control.Monad.Trans.Class (lift)

class MonadFix m => MonadSignalGen m where
    liftSignalGen :: SignalGen a -> m a

instance MonadSignalGen SignalGen where
    liftSignalGen = id

instance MonadSignalGen m => MonadSignalGen (IdentityT m) where
    liftSignalGen = lift . liftSignalGen

instance MonadSignalGen m => MonadSignalGen (MaybeT m) where
    liftSignalGen = lift . liftSignalGen

instance MonadSignalGen m => MonadSignalGen (ReaderT r m) where
    liftSignalGen = lift . liftSignalGen

instance (MonadSignalGen m, Monoid w) => MonadSignalGen (Lazy.WriterT w m) where
    liftSignalGen = lift . liftSignalGen

instance MonadSignalGen m => MonadSignalGen (Lazy.StateT s m) where
    liftSignalGen = lift . liftSignalGen

instance (MonadSignalGen m, Monoid w) => MonadSignalGen (Lazy.RWST r w s m) where
    liftSignalGen = lift . liftSignalGen

instance (MonadSignalGen m, Monoid w) => MonadSignalGen (Strict.WriterT w m) where
    liftSignalGen = lift . liftSignalGen

instance MonadSignalGen m => MonadSignalGen (Strict.StateT s m) where
    liftSignalGen = lift . liftSignalGen

instance (MonadSignalGen m, Monoid w) => MonadSignalGen (Strict.RWST r w s m) where
    liftSignalGen = lift . liftSignalGen

instance MonadSignalGen m => MonadSignalGen (ExceptT e m) where
    liftSignalGen = lift . liftSignalGen

-- | Same as 'FRP.Elerea.Simple.delay'
--
-- @delayS sig@ returns a 'Signal' whose value is equal to
-- the value of @sig@ in the previous step. This function
-- does not introduce a direct dependency; for example it
-- is ok if @sig@ depends on the resulting signal of the
-- call.
delayS :: MonadSignalGen m => a -> Signal a -> m (Signal a)
delayS a s = liftSignalGen (delay a s)

-- | Same as 'FRP.Elerea.Simple.generator'
--
-- @generatorS net@ runs the 'SignalGen' action specified
-- by @net@ each step. @generatorS@ returns a signal that
-- contains the value returned by the action in this step.
generatorS :: MonadSignalGen m => Signal (SignalGen a) -> m (Signal a)
generatorS = liftSignalGen . generator

-- | Same as 'FRP.Elerea.Simple.snapshot'
--
-- @snapshotS sig@ returns the current value of @sig@.
snapshotS :: MonadSignalGen m => Signal a -> m a
snapshotS = liftSignalGen . snapshot

-- | Same as 'FRP.Elerea.Simple.memo'
--
-- @memoS sig@ returns a memoized version of @sig@. The returned
-- signal can be used any number of times without the risk of
-- duplicated computation.
memoS :: MonadSignalGen m => Signal a -> m (Signal a)
memoS = liftSignalGen . memo

transferS :: MonadSignalGen m => a -> (t -> a -> a)-> Signal t -> m (Signal a)
transferS a k = liftSignalGen . transfer a k
