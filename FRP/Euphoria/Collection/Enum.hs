{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall #-}

-- | FRP.Euphoria.Collection.Generic, with an interface specialised for
-- "Enum" keys.
module FRP.Euphoria.Collection.Enum
    ( CollectionUpdate (..)
    , Collection
    -- * creating collections
    , simpleCollection
    , accumCollection
    , collectionToUpdates
    , emptyCollection
    , collectionFromList
    , collectionFromDiscreteList
    , makeCollection
    , mapToCollection
    -- * observing collections
    , watchCollection
    , followCollectionKey
    , collectionToDiscreteList
    , openCollection
    -- * other functions
    , mapCollection
    , mapCollectionWithKey
    , filterCollection
    , filterCollectionWithKey
    , justCollection
    , sequenceCollection
    ) where

import Data.EnumMap.Lazy (EnumMap)
import qualified Data.EnumMap.Lazy as EnumMap
import qualified Data.List as List
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Collection.Generic hiding (filterCollection
        , filterCollectionWithKey, justCollection, sequenceCollection
        , accumCollection, mapToCollection)
import qualified FRP.Euphoria.Collection.Generic as Gen
import FRP.Euphoria.Event

filterCollection
    :: (Enum k, MonadSignalGen m)
    => (a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollection =
    Gen.filterCollection (Proxy :: Proxy (EnumMap k))

filterCollectionWithKey
    :: (Enum k, MonadSignalGen m)
    => (k -> a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollectionWithKey =
    Gen.filterCollectionWithKey (Proxy :: Proxy (EnumMap k))

justCollection
    :: (Enum k, MonadSignalGen m)
    => Collection k (Maybe a)
    -> m (Collection k a)
justCollection =
    Gen.justCollection (Proxy :: Proxy (EnumMap k))

sequenceCollection
    :: (Enum k, MonadSignalGen m)
    => Collection k (SignalGen a)
    -> m (Collection k a)
sequenceCollection =
    Gen.sequenceCollection (Proxy :: Proxy (EnumMap k))

-- Adds the necessary state for holding the existing [(k, a)] and creating
-- the unique Event stream for each change of the collection.
accumCollection
    :: (Enum k, MonadSignalGen m)
    => Event (CollectionUpdate k a)
    -> m (Collection k a)
accumCollection =
    Gen.accumCollection (Proxy :: Proxy (EnumMap k))

mapToCollection
    :: (Eq k, Eq a, Enum k, MonadSignalGen m)
    => Discrete (EnumMap k a)
    -> m (Collection k (Discrete a))
mapToCollection =
    Gen.mapToCollection

-------------------------------------------------------------------------------
-- Creating Collections with sequential keys
--
-- These functions are unique to FRP.Euphoria.Collection.Enum, since they
-- actually require the Enum instance.

-- | A collection whose items are created by an event, and removed by
-- another event.
simpleCollection
    :: (Enum k, MonadSignalGen m)
    => k
    -- ^ The initial value for the unique keys. 'succ' will be used to get
    -- further keys.
    -> Event (a, Event ())
    -- ^ An Event that introduces a new item and its subsequent removal
    -- Event. The item will be removed from the collection when the Event
    -- () fires.
    -> m (Collection k a)
simpleCollection initialK evs =
    simpleCollectionUpdates initialK evs >>= accumCollection

simpleCollectionUpdates
    :: forall m k a. (Enum k, MonadSignalGen m) => k
    -> Event (a, Event ())
    -> m (Event (CollectionUpdate k a))
simpleCollectionUpdates initialK evs = do
    let addKey (a, ev) k = (succ k, (k, a, ev))
    newEvents <- scanAccumE initialK (addKey <$> evs)
    let addItem (k, _a, ev) = EnumMap.insert k ev
    rec
        removalEvent' <- delayE removalEvent
        removalEvents <- accumD EnumMap.empty
            ((addItem <$> newEvents) `mappend` (EnumMap.delete <$> removalEvent'))
        removalEvent <- switchD $ EnumMap.foldrWithKey
            (\k ev ev' -> (k <$ ev) `mappend` ev') mempty <$> removalEvents
    let updateAddItem :: (Enum k) => (k, a, Event ()) -> CollectionUpdate k a
        updateAddItem (k, a, _) = AddItem k a
    memoE $ (updateAddItem <$> newEvents) `mappend` (RemoveItem <$> removalEvent)

-- | A somewhat inefficient but easy-to-use way of turning a list of
-- items into a Collection. Probably should only be used for temporary
-- hacks. Will perform badly with large lists.
collectionFromDiscreteList
    :: (Enum k, Eq a, MonadSignalGen m)
    => k
    -> Discrete [a]
    -> m (Collection k a)
collectionFromDiscreteList initialK valsD = do
    valsE <- preservesD valsD
    evs <- scanAccumE (initialK, EnumMap.empty) (stepListCollState <$> valsE)
    accumCollection (flattenE evs)

-- This could obviously be implemented more efficiently.
stepListCollState :: (Enum k, Eq a) => [a]
                  -> (k, EnumMap k a)
                  -> ((k, EnumMap k a), [CollectionUpdate k a])
stepListCollState xs (initialK, existingMap) = ((k', newMap'), removeUpdates ++ addUpdates)
  where
    keyvals = EnumMap.toList existingMap
    newItems = xs List.\\ map snd keyvals
    removedKeys = map fst $ List.deleteFirstsBy
        (\(_, x) (_, y) -> x == y)
        keyvals
        (map (\x -> (initialK, x)) xs)
    (newMap, removeUpdates) = foldl
        (\(em, upds) k -> (EnumMap.delete k em, upds ++ [RemoveItem k]))
        (existingMap, []) removedKeys
    (k', newMap', addUpdates) = foldl
        (\(k, em, upds) x -> (succ k, EnumMap.insert k x em, upds ++ [AddItem k x]))
        (initialK, newMap, []) newItems
