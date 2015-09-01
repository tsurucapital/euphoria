{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

-- NOTE (asayers): I suppose this module doesn't really belong in
-- euphoria... but I don't think it deserves its own package.
module FRP.Euphoria.Internal.Maplike
    ( Maplike(..)
    , Lazy(..)
    , Strict(..)
    ) where

import Data.Function
import Data.Hashable (Hashable)
import qualified Data.Map.Lazy       as ML
import qualified Data.Map.Strict     as MS
import qualified Data.HashMap.Lazy   as HML
import qualified Data.HashMap.Strict as HMS
import qualified Data.EnumMap.Lazy   as EML
import qualified Data.EnumMap.Strict as EMS

newtype Lazy c k v = Lazy (c k v)
newtype Strict c k v = Strict (c k v)

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

instance Ord k => Maplike (Lazy ML.Map) k where
    union        (Lazy x) (Lazy y) = Lazy $ ML.union x y
    intersection (Lazy x) (Lazy y) = Lazy $ ML.intersection x y
    difference   (Lazy x) (Lazy y) = Lazy $ x ML.\\ y
    empty                          = Lazy ML.empty
    lookup       k (Lazy x)        = ML.lookup k x
    singleton    k v               = Lazy $ ML.singleton k v
    insert       k v (Lazy x)      = Lazy $ ML.insert k v x
    delete       k (Lazy x)        = Lazy $ ML.delete k x
    toList       (Lazy x)          = ML.toList x
    foldrWithKey f a (Lazy x)      = ML.foldrWithKey f a x

instance Ord k => Maplike (Strict MS.Map) k where
    union        (Strict x) (Strict y) = Strict $ MS.union x y
    intersection (Strict x) (Strict y) = Strict $ MS.intersection x y
    difference   (Strict x) (Strict y) = Strict $ x MS.\\ y
    empty                              = Strict MS.empty
    lookup       k (Strict x)          = MS.lookup k x
    singleton    k v                   = Strict $ MS.singleton k v
    insert       k v (Strict x)        = Strict $ MS.insert k v x
    delete       k (Strict x)          = Strict $ MS.delete k x
    toList       (Strict x)            = MS.toList x
    foldrWithKey f a (Strict x)        = MS.foldrWithKey f a x

instance Enum k => Maplike (Lazy EML.EnumMap) k where
    union        (Lazy x) (Lazy y) = Lazy $ EML.union x y
    intersection (Lazy x) (Lazy y) = Lazy $ EML.intersection x y
    difference   (Lazy x) (Lazy y) = Lazy $ x EML.\\ y
    empty                          = Lazy EML.empty
    lookup       k (Lazy x)        = EML.lookup k x
    singleton    k v               = Lazy $ EML.singleton k v
    insert       k v (Lazy x)      = Lazy $ EML.insert k v x
    delete       k (Lazy x)        = Lazy $ EML.delete k x
    toList       (Lazy x)          = EML.toList x
    foldrWithKey f a (Lazy x)      = EML.foldrWithKey f a x

instance Enum k => Maplike (Strict EMS.EnumMap) k where
    union        (Strict x) (Strict y) = Strict $ EMS.union x y
    intersection (Strict x) (Strict y) = Strict $ EMS.intersection x y
    difference   (Strict x) (Strict y) = Strict $ x EMS.\\ y
    empty                              = Strict EMS.empty
    lookup       k (Strict x)          = EMS.lookup k x
    singleton    k v                   = Strict $ EMS.singleton k v
    insert       k v (Strict x)        = Strict $ EMS.insert k v x
    delete       k (Strict x)          = Strict $ EMS.delete k x
    toList       (Strict x)            = EMS.toList x
    foldrWithKey f a (Strict x)        = EMS.foldrWithKey f a x

instance (Eq k, Hashable k) => Maplike (Lazy HML.HashMap) k where
    union        (Lazy x) (Lazy y) = Lazy $ HML.union x y
    intersection (Lazy x) (Lazy y) = Lazy $ HML.intersection x y
    difference   (Lazy x) (Lazy y) = Lazy $ HML.difference x y
    empty                          = Lazy HML.empty
    lookup       k (Lazy x)        = HML.lookup k x
    singleton    k v               = Lazy $ HML.singleton k v
    insert       k v (Lazy x)      = Lazy $ HML.insert k v x
    delete       k (Lazy x)        = Lazy $ HML.delete k x
    toList       (Lazy x)          = HML.toList x
    foldrWithKey f a (Lazy x)      = HML.foldrWithKey f a x

instance (Eq k, Hashable k) => Maplike (Strict HMS.HashMap) k where
    union        (Strict x) (Strict y) = Strict $ HMS.union x y
    intersection (Strict x) (Strict y) = Strict $ HMS.intersection x y
    difference   (Strict x) (Strict y) = Strict $ HMS.difference x y
    empty                              = Strict HMS.empty
    lookup       k (Strict x)          = HMS.lookup k x
    singleton    k v                   = Strict $ HMS.singleton k v
    insert       k v (Strict x)        = Strict $ HMS.insert k v x
    delete       k (Strict x)          = Strict $ HMS.delete k x
    toList       (Strict x)            = HMS.toList x
    foldrWithKey f a (Strict x)        = HMS.foldrWithKey f a x
