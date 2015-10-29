{-# OPTIONS_GHC -Wall #-}

-- | FRP.Euphoria.Internal.GenericCollection, with an interface specialised for
-- "Hashable" keys.
module FRP.Euphoria.HashCollection.Lazy
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
    , mapCollectionM
    , sequenceCollection
    ) where

import Data.Hashable (Hashable)
import Data.HashMap.Lazy (HashMap)
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Event
import qualified FRP.Euphoria.Internal.GenericCollection as Gen
import FRP.Euphoria.Internal.GenericCollection hiding (filterCollection
        , filterCollectionWithKey, justCollection, sequenceCollection
        , accumCollection, mapToCollection, simpleCollection
        , collectionFromDiscreteList, mapCollectionM)
import FRP.Euphoria.Internal.Maplike

filterCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => (a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollection =
    Gen.filterCollection (Proxy :: Proxy (Lazy HashMap k))

filterCollectionWithKey
    :: (Hashable k, Eq k, MonadSignalGen m)
    => (k -> a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollectionWithKey =
    Gen.filterCollectionWithKey (Proxy :: Proxy (Lazy HashMap k))

justCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Collection k (Maybe a)
    -> m (Collection k a)
justCollection =
    Gen.justCollection (Proxy :: Proxy (Lazy HashMap k))

mapCollectionM
    :: (Eq k, Hashable k, MonadSignalGen m)
    => (a -> SignalGen b) -> Collection k a -> m (Collection k b)
mapCollectionM = Gen.mapCollectionM (Proxy :: Proxy (Lazy HashMap k))

sequenceCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Collection k (SignalGen a)
    -> m (Collection k a)
sequenceCollection =
    Gen.sequenceCollection (Proxy :: Proxy (Lazy HashMap k))

-- Adds the necessary state for holding the existing [(k, a)] and creating
-- the unique Event stream for each change of the collection.
accumCollection
    :: (Hashable k, Eq k, MonadSignalGen m)
    => Event (CollectionUpdate k a)
    -> m (Collection k a)
accumCollection =
    Gen.accumCollection (Proxy :: Proxy (Lazy HashMap k))

mapToCollection
    :: (Eq k, Eq a, Hashable k, MonadSignalGen m)
    => Discrete (HashMap k a)
    -> m (Collection k (Discrete a))
mapToCollection xsD =
    Gen.mapToCollection (Lazy <$> xsD)

simpleCollection
    :: (Enum k, Eq k, Hashable k, MonadSignalGen m)
    => k
    -> Event (a, Event ())
    -> m (Collection k a)
simpleCollection =
    Gen.simpleCollection (Proxy :: Proxy (Lazy HashMap k))

collectionFromDiscreteList
    :: (Enum k, Eq k, Hashable k, Eq a, MonadSignalGen m)
    => k
    -> Discrete [a]
    -> m (Collection k a)
collectionFromDiscreteList =
    Gen.collectionFromDiscreteList (Proxy :: Proxy (Lazy HashMap k))
