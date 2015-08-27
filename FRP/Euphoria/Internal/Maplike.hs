{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- NOTE (asayers): I suppose this module doesn't really belong in
-- euphoria... but I don't think it deserves its own package.
module FRP.Euphoria.Internal.Maplike
    ( Maplike(..)
    ) where

import Data.Hashable (Hashable)
import qualified Data.Map            as Map
import qualified Data.HashMap.Strict as HMS
import qualified Data.EnumMap.Lazy   as EML

-- | A class for types with an API similar to that of "Data.Map".
class Maplike c k where
    union        :: c k v -> c k v -> c k v
    intersection :: c k v -> c k v -> c k v
    difference   :: c k v -> c k v -> c k v
    empty        :: c k v
    lookup       :: k -> c k v -> Maybe v
    singleton    :: k -> v -> c k v
    singleton k v = insert k v empty
    insert :: k -> v -> c k v -> c k v
    insert k v m = singleton k v `union` m
    delete :: k -> c k v -> c k v
    delete k m =  m `difference` singleton k (error "bug")
    toList :: c k v -> [(k, v)]
    foldrWithKey :: (k -> v -> a -> a) -> a -> c k v -> a

instance Ord k => Maplike Map.Map k where
    union        = Map.union
    intersection = Map.intersection
    difference   = (Map.\\)
    empty        = Map.empty
    lookup       = Map.lookup
    singleton    = Map.singleton
    insert       = Map.insert
    delete       = Map.delete
    toList       = Map.toList
    foldrWithKey = Map.foldrWithKey

instance Enum k => Maplike EML.EnumMap k where
    union        = EML.union
    intersection = EML.intersection
    difference   = (EML.\\)
    empty        = EML.empty
    lookup       = EML.lookup
    singleton    = EML.singleton
    insert       = EML.insert
    delete       = EML.delete
    toList       = EML.toList
    foldrWithKey = EML.foldrWithKey

instance (Eq k, Hashable k) => Maplike HMS.HashMap k where
    union        = HMS.union
    intersection = HMS.intersection
    difference   = HMS.difference
    empty        = HMS.empty
    lookup       = HMS.lookup
    singleton    = HMS.singleton
    insert       = HMS.insert
    delete       = HMS.delete
    toList       = HMS.toList
    foldrWithKey = HMS.foldrWithKey
