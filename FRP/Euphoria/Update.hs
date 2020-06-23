{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Signals for incremental updates.
module FRP.Euphoria.Update
    ( Update(..)
    , updateUseAll
    , updateUseLast
    , updateUseAllIO
    , discreteToUpdate
    , mappendUpdateIO
    , startUpdateNetwork
    , startUpdateNetworkWithValue
    ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Semigroup
import Data.These
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
data Update a = forall s. (Semigroup s) => Update (s -> a) (Event s)

instance Functor Update where
    f `fmap` Update final evt = Update (f . final) evt

instance (Semigroup a) => Semigroup (Update a) where
    Update f x <> Update g y = Update
        (these f g (\a b -> f a <> g b))
        (fmap This x <> fmap That y)

    stimes n = fmap (stimes n)

instance (Monoid a) => Monoid (Update a) where
    mempty = Update (const mempty) (mempty :: Event ())

-- | Convert an 'Event' to an 'Update' by combining the occurrences,
-- i.e. without doing any shortcut.
updateUseAll :: (Semigroup a) => Event a -> Update a
updateUseAll evt = Update id evt

-- | Create an 'Update' that ignores all but the latest occurrences.
updateUseLast :: Event a -> Update a
updateUseLast evt = Update getLast (Last <$> evt)

-- | > discreteToUpdate d = fmap updateUseLast (preservesD d)
discreteToUpdate :: MonadSignalGen m => Discrete a -> m (Update a)
discreteToUpdate aD = updateUseLast <$> preservesD aD

-- | Do the same thing as 'updateUseAll' but use (>>) in place of mappend.
updateUseAllIO :: Semigroup a => Event (IO a) -> Update (IO a)
updateUseAllIO = updateUseAll

-- | Do the same thing as 'mappend' but use (>>) in place of mappend.
mappendUpdateIO :: Semigroup a => Update (IO a) -> Update (IO a) -> Update (IO a)
mappendUpdateIO = (<>)
{-# RULES "mappendUpdateIO/()" mappendUpdateIO = mappendUpdateIOUnit #-}
{-# INLINE[0] mappendUpdateIO #-}

-- | Do the same thing as 'mappendUpdateIO' but specialized to 'IO ()'
mappendUpdateIOUnit :: Update (IO ()) -> Update (IO ()) -> Update (IO ())
mappendUpdateIOUnit = (<>)

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
                    DUS currentToFinal current (accCurrent <> x') accFinal
            where
                x' = unsafeCoerce x

newDynUpdateState :: (Monoid a) => IO (DynUpdateState a)
newDynUpdateState = do
    u <- newUnique
    return $! DUS (const mempty) u () mempty

type DynUpdate a = Dual (Endo (DynUpdateState a))
data DynUpdateState a =
    forall s{-current underlying semigroup-}. (Semigroup s) => DUS
        (s -> a) -- how to turn the current semigroup into the final type
        !Unique -- unique id for the current underlying Update
        !s -- accumulated current semigroup
        !a -- accumulated final result

-- | Execute a network whose output is represented with an 'Update'.
-- It returns 2 actions, a sampling action and a stepping action.
-- The stepping action executes one cycle of the network, updating
-- its internal state. The sampling action first steps the network,
-- then observes the final 'Update' value. It returns the
-- combined value corresponding to the interval between now and the
-- last time the sampling action was executed.
startUpdateNetwork
    :: SignalGen (Update a)
    -> IO (IO (Maybe a), IO ())
startUpdateNetwork network = do
    (sample, step) <- startUpdateNetworkWithValue network'
    return (fst <$> sample, step)
    where
        network' = flip (,) (pure ()) <$> network

-- | This is 'IORef (Maybe a)', plus the ability to wipe the underlying
-- value. We use this in 'startUpdateNetworkWithValue' because we need
-- to hide the type of the underlying state, but we want to retain the
-- ability to clear the changeset when we sample the network.
data Updoot a = forall s. Updoot (s -> a) (IORef (Maybe s))

-- | Execute a network that has both a continuous output and an
-- accumulated updates.
startUpdateNetworkWithValue :: forall a b.
    SignalGen (Update a, Signal b) -> IO (IO (Maybe a, b), IO b)
startUpdateNetworkWithValue network = do

    -- Contains the value from the signal. We write to this on every
    -- sample, so it should never be empty.
    valueRef :: IORef b
        <- newIORef (error "valueRef empty")

    -- Contains the updoot from the update. We write to this when we
    -- initialising the network below.
    updootRef :: IORef (Updoot a)
        <- newIORef (error "updootRef empty")

    sample <- start $ do
        (Update final updateE, signal) <- network

        -- Contains the accumulated change since the last 'takeChange'.
        changeRef <- liftIO $ newIORef Nothing

        -- This is why we need 'Updoot': If we returned
        -- (final, changeRef) here the type of the underlying state
        -- would be leaked to the outer scope.
        liftIO $ writeIORef updootRef $ Updoot final changeRef

        let updateChange :: Signal (IO ())
            updateChange = writeUpdate changeRef <$> eventToSignal updateE

            updateValue :: Signal (IO ())
            updateValue = writeIORef valueRef <$> signal

        pure $ (>>) <$> updateChange <*> updateValue

    updoot <- readIORef updootRef

    return
        ( join sample >> (,) <$> takeChange updoot <*> readIORef valueRef
        , join sample >> readIORef valueRef
        )
    where
        -- Accumulate the new changes and append them to the existing
        -- change, if it exists. We apply new changes on the right.
        writeUpdate :: Semigroup s => IORef (Maybe s) -> [s] -> IO ()
        writeUpdate changeRef (mconcat . fmap Just -> newUpdate) =
            modifyIORef' changeRef (<> newUpdate)

        -- Read and clear the accumulated change.
        takeChange :: Updoot a -> IO (Maybe a)
        takeChange (Updoot final changeRef) =
            atomicModifyIORef' changeRef $ (Nothing,) . fmap final
