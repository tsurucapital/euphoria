{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Signals for incremental updates.
module FRP.Euphoria.Update
    ( Update(..)
    , updateUseAll
    , updateUseLast
    , updateUseAllIO
    , stepperUpdate
    , discreteToUpdate
    , mappendUpdateIO
    , startUpdateNetwork
    , startUpdateNetworkWithValue

    , IOMonoid(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Semigroup hiding (Last(..))
import Data.Monoid hiding ((<>))
import Data.Unique
import Unsafe.Coerce

import FRP.Euphoria.Event

-- | @Update a@ represents a stream of events, just like an 'Event'.
-- Unlike an 'Event', you cannot observe individual event ocurrences;
-- you first specify a time interval, and you will receive data
-- made by combining together all occurrences in that interval.
-- The type @a@ represents those combined data.
--
-- A typical usage is to update external objects in batch.
-- For example, suppose you have @(data :: 'Discrete' 'String')@ which
-- you want to display on a GUI window. The simplest way to do
-- this would be to use 'changesD' to obtain a event stream of
-- all changes to @data@, then use fmap to construct a stream of update actions
-- of type @'Event' (IO ())@, which will be executed one by one.
-- However, this becomes wasteful if @data@ changes more frequently
-- than you want to update the window, for example you only update the
-- window once in a few network steps. This is because all but the last
-- update operation will be immediately overwritten and have no effect.
--
-- A better way here is to create an @Update (IO ())@ which gives
-- no more than 1 operation when sampled, corresponding to the last change
-- of the underlying data. To do this you first apply 'updateUseLast'
-- to the event stream of changes, then use fmap to construct an
-- @Update (IO ())@.
--
-- Note: there is no way to construct a 'Signal', 'Event', or 'Discrete'
-- that depends on an 'Update'. The only way to extract information
-- from an 'Update' is 'startUpdateNetwork'.
--
-- Note: in the current implementation, if you use an 'Update' twice,
-- an unbounded amount of computation can be duplicated. Please
-- avoid doing so.
data Update a = forall s. (Monoid s) => Update (s -> a) (Event s)

instance Functor Update where
    f `fmap` Update final evt = Update (f . final) evt

instance Applicative Update where
    pure x = Update (const x) (mempty :: Event ())
    Update f_final f_evt <*> Update a_final a_evt = Update
        (\(f_s, a_s) -> f_final f_s (a_final a_s))
        ((left <$> f_evt) `mappend` (right <$> a_evt))
        where
            left f = (f, mempty)
            right a = (mempty, a)

instance Monoid a => Semigroup (Update a) where
  Update f x <> Update g y = Update
      (\(s0, s1) -> f s0 `mappend` g s1)
      ((left <$> x) <> (right <$> y))
      where
          left val = (val, mempty)
          right val = (mempty, val)

instance (Monoid a) => Monoid (Update a) where
    mempty = Update (\() -> mempty) mempty

-- | Convert an 'Event' to an 'Update' by combining the occurrences,
-- i.e. without doing any shortcut.
updateUseAll :: (Monoid a) => Event a -> Update a
updateUseAll evt = Update id evt

-- | Create an 'Update' that ignores all but the latest occurrences.
updateUseLast :: Event a -> Update (Maybe a)
updateUseLast evt = Update getLast (Last . Just <$> evt)

-- is it useful?
stepperUpdate :: a -> Event a -> Update a
stepperUpdate initial aE = fromMaybe initial <$> updateUseLast aE

-- | > discreteToUpdate d = fmap updateUseLast (preservesD d)
discreteToUpdate :: MonadSignalGen m => Discrete a -> m (Update (Maybe a))
discreteToUpdate aD = updateUseLast <$> preservesD aD

-- | Do the same thing as 'updateUseAll' but use (>>) in place of mappend.
updateUseAllIO :: Monoid a => Event (IO a) -> Update (IO a)
updateUseAllIO ioE = unIOMonoid <$> updateUseAll (IOMonoid <$> ioE)

-- | Do the same thing as 'mappend' but use (>>) in place of mappend.
mappendUpdateIO :: Monoid a => Update (IO a) -> Update (IO a) -> Update (IO a)
mappendUpdateIO d1 d2 = unIOMonoid <$> ((IOMonoid <$> d1) `mappend` (IOMonoid <$> d2))
{-# RULES "mappendUpdateIO/()" mappendUpdateIO = mappendUpdateIOUnit #-}
{-# INLINE[0] mappendUpdateIO #-}

-- | Do the same thing as 'mappendUpdateIO' but specialized to 'IO ()'
mappendUpdateIOUnit :: Update (IO ()) -> Update (IO ()) -> Update (IO ())
mappendUpdateIOUnit = liftA2 (>>)

instance (Monoid a) => SignalSet (Update a) where
    basicSwitchD dis = do
        updatesE <- preservesD dis
        dynUpdatesE <- mapEIO mkDynUpdates updatesE
        dynUpdatesD <- stepperD undefined dynUpdatesE
        dynE <- switchD dynUpdatesD
        initial <- liftSignalGen $ execute newDynUpdateState
        return $ Update (applyDynUpdates initial) dynE
        where
            applyDynUpdates initial (Dual (Endo f)) = case f initial of
                DUS toFinal _ acc accFinal -> accFinal `mappend` toFinal acc
    memoizeSignalSet = return -- There is no effective way to memoize it.

mkDynUpdates :: (Monoid a) => Update a -> IO (Event (DynUpdate a))
mkDynUpdates _upd@(Update toFinal evt) = do
    u <- newUnique
    return $ toUpdate u <$> evt
    where
        toUpdate u x = Dual $ Endo $ \(DUS currentToFinal current accCurrent accFinal) ->
            if current /= u
                then-- The current underlying is different from _upd.
                    -- So we finalize the current accumulator and
                    -- set _upd as the current underlying.
                    DUS toFinal u x (mappend accFinal (currentToFinal accCurrent))
                else-- The current underlying is already the same as _upd.
                    -- This means accCurrent is of the same type as x.
                    -- We add x to the current accumulator.
                    DUS currentToFinal current (mappend accCurrent x') accFinal
            where
                x' = unsafeCoerce x

newDynUpdateState :: (Monoid a) => IO (DynUpdateState a)
newDynUpdateState = do
    u <- newUnique
    return $! DUS (const mempty) u () mempty

type DynUpdate a = Dual (Endo (DynUpdateState a))
data DynUpdateState a =
    forall s{-current underlying monoid-}. (Monoid s) => DUS
        (s -> a) -- how to turn the current monoid into the final type
        !Unique -- unique id for the current underlying Update
        !s -- accumulated current monoid
        !a -- accumulated final result

newtype IOMonoid a = IOMonoid {unIOMonoid :: IO a}

instance Semigroup a => Semigroup (IOMonoid a) where
    IOMonoid m <> IOMonoid n = IOMonoid $ (<>) <$> m <*> n

instance (Monoid a) => Monoid (IOMonoid a) where
    mempty = IOMonoid (return mempty)

data Changes a = forall s. (Monoid s) => Changes (s -> a) s

-- | Execute a network whose output is represented with an 'Update'.
-- It returns 2 actions, a sampling action and a stepping action.
-- The stepping action executes one cycle of the network, updating
-- its internal state. The sampling action first steps the network,
-- then observes the final 'Update' value. It returns the
-- combined value corresponding to the interval between now and the
-- last time the sampling action was executed.
startUpdateNetwork
    :: SignalGen (Update a)
    -> IO (IO a, IO ())
startUpdateNetwork network = do
    (sample, step) <- startUpdateNetworkWithValue network'
    return (fst <$> sample, step)
    where
        network' = flip (,) (pure ()) <$> network

-- | Execute a network that has both a continuous output and an
-- accumulated updates.
startUpdateNetworkWithValue :: SignalGen (Update a, Signal b) -> IO (IO (a, b), IO b)
startUpdateNetworkWithValue network = do
    changesRef <- newIORef Nothing
    valueRef <- newIORef undefined
    -- IORef (Maybe Changes)
    sample <- start $ do
        (update, signal) <- network
        case update of
            Update final updateE -> return $
                (>>) <$> updateChanges <*> updateVal
                where
                    updateChanges = updateRef changesRef final <$> eventToSignal updateE
                    updateVal = writeIORef valueRef <$> signal
    return (join sample >> readBoth valueRef changesRef, join sample >> readIORef valueRef)
    where
        updateRef changesRef final occs = do
            changes <- readIORef changesRef
            writeIORef changesRef $! Just $! case changes of
                Nothing -> Changes final newChanges
                Just (Changes _ oldChanges) ->
                    let !allChanges = unsafeCoerce oldChanges `mappend` newChanges
                        -- FIXME: I believe it's possible to avoid unsafeCoerce here (akio)
                    in Changes final allChanges
            where !newChanges = mconcat occs

        readBoth valueRef changesRef =
            (,) <$> takeChanges changesRef
                <*> readIORef valueRef

        takeChanges changesRef = do
            changes <- readIORef changesRef
            case changes of
                Nothing -> error "FRP.Elerea.Extras.Update: bug: no changes"
                Just (Changes final oldChanges) -> do
                    writeIORef changesRef Nothing
                    return $! final oldChanges
