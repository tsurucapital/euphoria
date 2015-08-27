{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Collection.Generic hiding (filterCollection
        , filterCollectionWithKey, justCollection, sequenceCollection
        , accumCollection, mapToCollection, simpleCollection
        , collectionFromDiscreteList)
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

simpleCollection
    :: (Enum k, MonadSignalGen m)
    => k
    -> Event (a, Event ())
    -> m (Collection k a)
simpleCollection =
    Gen.simpleCollection (Proxy :: Proxy (EnumMap k))

collectionFromDiscreteList
    :: (Enum k, Eq a, MonadSignalGen m)
    => k
    -> Discrete [a]
    -> m (Collection k a)
collectionFromDiscreteList =
    Gen.collectionFromDiscreteList (Proxy :: Proxy (EnumMap k))
