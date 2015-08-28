
-- | A re-export of FRP.Euphoria.Collection.Enum for API compatability.
-- `Collection.Enum.mapToCollection` is renamed to `enummapToCollection`,
-- and `Collection.Hashable.mapToCollection` is exported as
-- `hashmapToCollection`.
module FRP.Euphoria.Collection
    ( module FRP.Euphoria.Collection.Enum
    , enummapToCollection
    , hashmapToCollection
    ) where

import Data.EnumMap.Lazy (EnumMap)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

import FRP.Euphoria.Collection.Enum hiding (mapToCollection)

import qualified FRP.Euphoria.Collection.Enum     as Enum
import qualified FRP.Euphoria.Collection.Hashable as Hashable

import FRP.Euphoria.Event

enummapToCollection
    :: (Eq k, Eq a, Enum k, MonadSignalGen m)
    => Discrete (EnumMap k a)
    -> m (Collection k (Discrete a))
enummapToCollection = Enum.mapToCollection

hashmapToCollection
    :: (Eq k, Eq a, Hashable k, MonadSignalGen m)
    => Discrete (HashMap k a)
    -> m (Collection k (Discrete a))
hashmapToCollection = Hashable.mapToCollection


