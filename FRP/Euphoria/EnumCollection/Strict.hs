{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | FRP.Euphoria.Internal.GenericCollection, with an interface specialised for
-- "Enum" keys.
module FRP.Euphoria.EnumCollection.Strict
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

import Data.EnumMap.Strict (EnumMap)
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Event
import qualified FRP.Euphoria.Internal.GenericCollection as Gen
import FRP.Euphoria.Internal.GenericCollection hiding (filterCollection
        , filterCollectionWithKey, justCollection, sequenceCollection
        , accumCollection, mapToCollection, simpleCollection
        , collectionFromDiscreteList, mapCollectionM)
import FRP.Euphoria.Internal.Maplike

filterCollection
    :: (Enum k, MonadSignalGen m)
    => (a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollection =
    Gen.filterCollection (Proxy :: Proxy (Strict EnumMap k))

filterCollectionWithKey
    :: (Enum k, MonadSignalGen m)
    => (k -> a -> Bool)
    -> Collection k a
    -> m (Collection k a)
filterCollectionWithKey =
    Gen.filterCollectionWithKey (Proxy :: Proxy (Strict EnumMap k))

justCollection
    :: (Enum k, MonadSignalGen m)
    => Collection k (Maybe a)
    -> m (Collection k a)
justCollection =
    Gen.justCollection (Proxy :: Proxy (Strict EnumMap k))

mapCollectionM
    :: (Enum k, MonadSignalGen m)
    => (a -> SignalGen b) -> Collection k a -> m (Collection k b)
mapCollectionM = Gen.mapCollectionM (Proxy :: Proxy (Strict EnumMap k))

sequenceCollection
    :: (Enum k, MonadSignalGen m)
    => Collection k (SignalGen a)
    -> m (Collection k a)
sequenceCollection =
    Gen.sequenceCollection (Proxy :: Proxy (Strict EnumMap k))

-- Adds the necessary state for holding the existing [(k, a)] and creating
-- the unique Event stream for each change of the collection.
accumCollection
    :: (Enum k, MonadSignalGen m)
    => Event (CollectionUpdate k a)
    -> m (Collection k a)
accumCollection =
    Gen.accumCollection (Proxy :: Proxy (Strict EnumMap k))

mapToCollection
    :: (Eq k, Eq a, Enum k, MonadSignalGen m)
    => Discrete (EnumMap k a)
    -> m (Collection k (Discrete a))
mapToCollection xsD =
    Gen.mapToCollection (Strict <$> xsD)

simpleCollection
    :: (Enum k, MonadSignalGen m)
    => k
    -> Event (a, Event ())
    -> m (Collection k a)
simpleCollection =
    Gen.simpleCollection (Proxy :: Proxy (Strict EnumMap k))

collectionFromDiscreteList
    :: (Enum k, Eq a, MonadSignalGen m)
    => k
    -> Discrete [a]
    -> m (Collection k a)
collectionFromDiscreteList =
    Gen.collectionFromDiscreteList (Proxy :: Proxy (Strict EnumMap k))
