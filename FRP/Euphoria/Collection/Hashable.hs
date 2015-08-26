{-# OPTIONS_GHC -Wall #-}

-- | FRP.Euphoria.Collection.Generic, with an interface specialised for
-- "Hashable" keys.
module FRP.Euphoria.Collection.Hashable
    ( CollectionUpdate (..)
    , Collection
    -- * creating collections
    , accumCollection
    , collectionToUpdates
    , emptyCollection
    , collectionFromList
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

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Collection.Generic hiding (filterCollection
        , filterCollectionWithKey, justCollection, sequenceCollection
        , accumCollection, mapToCollection)
import qualified FRP.Euphoria.Collection.Generic as Gen
import FRP.Euphoria.Event

filterCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => (a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollection =
    Gen.filterCollection (Proxy :: Proxy (HashMap k))

filterCollectionWithKey
    :: (Hashable k, Eq k, MonadSignalGen m)
    => (k -> a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollectionWithKey =
    Gen.filterCollectionWithKey (Proxy :: Proxy (HashMap k))

justCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Collection k (Maybe a)
    -> m (Collection k a)
justCollection =
    Gen.justCollection (Proxy :: Proxy (HashMap k))

sequenceCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Collection k (SignalGen a)
    -> m (Collection k a)
sequenceCollection =
    Gen.sequenceCollection (Proxy :: Proxy (HashMap k))

-- Adds the necessary state for holding the existing [(k, a)] and creating
-- the unique Event stream for each change of the collection.
accumCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Event (CollectionUpdate k a)
    -> m (Collection k a)
accumCollection =
    Gen.accumCollection (Proxy :: Proxy (HashMap k))

mapToCollection
    :: (Eq k, Eq a, Hashable k, MonadSignalGen m)
    => Discrete (HashMap k a)
    -> m (Collection k (Discrete a))
mapToCollection =
    Gen.mapToCollection
