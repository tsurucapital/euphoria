{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

#if __GLASGOW_HASKELL__ <= 706
{-# LANGUAGE DoRec #-}
#else
{-# LANGUAGE RecursiveDo #-}
#endif

-- | Collection signals with incremental updates.
module FRP.Euphoria.Collection
( CollectionUpdate (..)
, Collection
-- * creating collections
, simpleCollection
, accumCollection
, collectionToUpdates
, emptyCollection
, collectionFromList
, collectionFromDiscreteList
, mapToCollection
, makeCollection
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

import Control.Applicative
import Control.Monad (join)
import Data.EnumMap.Lazy (EnumMap)
import qualified Data.EnumMap.Lazy as EnumMap
import Data.List
import Data.Maybe (mapMaybe)
import Data.Traversable
import Data.Foldable (Foldable)
import Data.Monoid

import FRP.Euphoria.Event


-- | Represents an incremental change to a collection of items.
data CollectionUpdate k a
    = AddItem k a
    | RemoveItem k
    deriving (Functor, Eq, Show, Foldable, Traversable)

-- | An FRP interface for representing an incrementally updated
-- collection of items. The items are identified by a unique key.
-- Items may be added or removed from the current collection.
--
-- This type is useful because it allows you to manage the incremental
-- state updates to something that needs a collection of items without
-- having to rebuild it completely every time the collection changes.
-- Consider the type Signal [a] -- functionally, it also represents a
-- collection of items that changes over time. However, there is no
-- state carried between changes. If, for example, we have a GUI
-- widget that lists items whose content is represented as a Signal
-- [a], we would have to destroy and rebuild the widget's internal
-- state every time the list contents change. But with the Collection
-- type, we can add or remove from the GUI widget only the necessary
-- items. This is useful both from a performance (most existing GUI
-- toolkits exhibit worse performance when adding and removing all
-- items with every change) and behavior standpoint, because the GUI
-- toolkit can, for example, remember which items the user had
-- selected between list updates.
--
-- Usage of 'Collection' implies there could be some caching/state by
-- the consumer of the Events, otherwise one might as well use a
-- Signal [a].
newtype Collection k a = Collection {
  unCollection :: Discrete ([(k, a)], Event (CollectionUpdate k a))
  }

instance SignalSet (Collection k a) where
    basicSwitchD dis0 = do
        dis <- memoD dis0
        listD <- memoD $ join (fmap fst . unCollection <$> dis)
        listS <- discreteToSignal listD
        prevListS <- delayS [] listS

        chE <- dropStepE $ changesD dis
        (_, initialUpdatesE) <- openCollection =<< snapshotD dis
        updatesE <- generatorD' =<< stepperD (return initialUpdatesE)
            (updates <$> prevListS <*> listS <@> chE)

        makeCollection listD updatesE
        where
            updates prevList list (Collection newCol) = do
                rebuild <- flattenE <$> onCreation (map remove prevList ++ map add list)
                (_, newUpdates) <- snapshotD newCol
                memoE $ rebuild `mappend` newUpdates
            remove (k, _) = RemoveItem k
            add (k, v) = AddItem k v

    memoizeSignalSet (Collection dis)= Collection <$> memoD dis

-- | Like 'fmap', but the Collection and interior 'Event' stream are memoized
mapCollection :: (a -> b) -> Collection k a -> SignalGen (Collection k b)
mapCollection = mapCollectionWithKey . const

-- | A version of 'mapCollection' which provides access to the key
mapCollectionWithKey :: (k -> a -> b) -> Collection k a -> SignalGen (Collection k b)
mapCollectionWithKey f aC = do
    updateE    <- snd <$> openCollection aC
    newCurD    <- memoD $ fmap (fmap ft . fst) $ unCollection aC
    newUpdateE <- memoE $ fmap fcu updateE
    makeCollection newCurD newUpdateE
  where
    -- f applied to tuples and collection updates
    ft (k, x)          = (k, f k x)
    fcu (AddItem k x)  = AddItem k (f k x)
    fcu (RemoveItem k) = RemoveItem k

filterCollection :: (Enum k) => (a -> Bool) -> Collection k a -> SignalGen (Collection k a)
filterCollection = filterCollectionWithKey . const

filterCollectionWithKey :: forall k a. (Enum k) => (k -> a -> Bool) -> Collection k a -> SignalGen (Collection k a)
filterCollectionWithKey f aC = mapCollectionWithKey f' aC >>= justCollection where
    f' k v
        | f k v = Just v
        | otherwise = Nothing

justCollection :: forall k a. (Enum k) => Collection k (Maybe a) -> SignalGen (Collection k a)
-- Inefficient, quick-hack implementation
justCollection c = do
    upds <- collectionToUpdates c
    let f :: CollectionUpdate k (Maybe a) -> EnumMap k () -> (EnumMap k (), Maybe (CollectionUpdate k a))
        f (AddItem k Nothing) m = (EnumMap.insert k () m, Nothing)
        f (AddItem k (Just a)) m = (m, Just (AddItem k a))
        f (RemoveItem k) m = case EnumMap.lookup k m of
            Just () -> (EnumMap.delete k m, Nothing)
            Nothing -> (m, Just (RemoveItem k))
    upds' <- scanAccumE EnumMap.empty (f <$> upds)
    accumCollection =<< memoE (justE upds')

-- | Create an 'Event' stream of all updates from a collection, including
-- the items currently in it.
collectionToUpdates
    :: forall k a. Collection k a
    -> SignalGen (Event (CollectionUpdate k a))
collectionToUpdates aC = do
    (cur,updateE) <- openCollection aC
    initE  <- onCreation (map (uncurry AddItem) cur)
    initE' <- memoE $ flattenE initE
    return (updateE `mappend` initE')

sequenceCollection
    :: Enum k
    => Collection k (SignalGen a)
    -> SignalGen (Collection k a)
sequenceCollection col = collectionToUpdates col
  >>= generatorE . fmap sequenceA
  >>= accumCollection

-- | A collection whose items are created by an event, and removed by
-- another event.
simpleCollection :: (Enum k)
                 => k
                 -- ^ The initial value for the unique keys. 'succ'
                 -- will be used to get further keys.
                 -> Event (a, Event ())
                 -- ^ An Event that introduces a new item and its
                 -- subsequent removal Event. The item will be removed
                 -- from the collection when the Event () fires.
                 -> SignalGen (Collection k a)
simpleCollection initialK evs =
    simpleCollectionUpdates initialK evs >>= accumCollection

simpleCollectionUpdates :: (Enum k) => k
                        -> Event (a, Event ())
                        -> SignalGen (Event (CollectionUpdate k a))
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
    let -- updateAddItem :: (Enum k) => (k, a, Event ()) -> CollectionUpdate k a
        updateAddItem (k, a, _) = AddItem k a
    memoE $ (updateAddItem <$> newEvents) `mappend` (RemoveItem <$> removalEvent)

-- Turns adds the necessary state for holding the existing [(k, a)]
-- and creating the unique Event stream for each change of the
-- collection.
accumCollection :: (Enum k)
                => Event (CollectionUpdate k a)
                -> SignalGen (Collection k a)
accumCollection ev = do
    let toMapOp (AddItem k a) = EnumMap.insert k a
        toMapOp (RemoveItem k) = EnumMap.delete k
    mapping <- accumD EnumMap.empty (toMapOp <$> ev)
    listD <- memoD $ EnumMap.toList <$> mapping
    makeCollection listD ev

-- Adding elemenets is faster than "accumCollection", but deleting them is
-- slower. Note that the semantics differ in the case of adding the same
-- key multiple times. "accumCollection" replaces, whereas
-- "genericAccumCollection" allows multiple copies.
genericAccumCollection
    :: (Eq k)
    => Event (CollectionUpdate k a)
    -> SignalGen (Collection k a)
genericAccumCollection ev = do
    let toMapOp update = case update of
            AddItem k a   -> (:) (k, a)
            RemoveItem k1 -> filter (\(k2, _) -> k1 /= k2)
    listD <- memoD =<< accumD [] (toMapOp <$> ev)
    makeCollection listD ev

-- | The primitive interface for creating a 'Collection'. The two
-- arguments must be coherent, i.e. the value of the discrete at
-- time /t+1/ should be obtained by applying the updates
-- at /t+1/ to the value of the discrete at /t/. This invariant
-- is not checked.
makeCollection
    :: Discrete [(k, a)]
    -> Event (CollectionUpdate k a)
    -> SignalGen (Collection k a)
makeCollection listD updE = Collection <$> generatorD (gen <$> listD)
    where
        gen list = do
            updE' <- dropStepE updE
            return (list, updE')

-- | Prints add/remove diagnostics for a Collection. Useful for debugging
watchCollection :: (Show k, Show a)
                => Collection k a -> SignalGen (Event (IO ()))
watchCollection (Collection coll) = do
    ev1 <- takeE 1 =<< preservesD coll
    now <- onCreation ()
    let f (items, ev) = ((putStrLn . showUpdate) <$> ev) `mappend`
            (mapM_ (putStrLn . showExisting) items <$ now)
        showUpdate (AddItem k a) = "Add: " ++ show k ++ ", " ++ show a
        showUpdate (RemoveItem k) = "Remove: " ++ show k
        showExisting (k, a) = "Existing: " ++ show k ++ ", " ++ show a
    switchD =<< stepperD mempty (f <$> ev1)

-- | An empty, unchanging Collection.
emptyCollection :: Collection k a
emptyCollection = collectionFromList []

-- | A pure function to create a Collection from key-value pairs. This
-- collection will never change.
collectionFromList :: [(k, a)] -> Collection k a
collectionFromList kvs = Collection $ pure (kvs, mempty)

-- | A somewhat inefficient but easy-to-use way of turning a list of
-- items into a Collection. Probably should only be used for temporary
-- hacks. Will perform badly with large lists.
collectionFromDiscreteList
    :: (Enum k, Eq a)
    => k
    -> Discrete [a]
    -> SignalGen (Collection k a)
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
    newItems = xs \\ map snd keyvals
    removedKeys = map fst $ deleteFirstsBy
        (\(_, x) (_, y) -> x == y)
        keyvals
        (map (\x -> (initialK, x)) xs)
    (newMap, removeUpdates) = foldl
        (\(em, upds) k -> (EnumMap.delete k em, upds ++ [RemoveItem k]))
        (existingMap, []) removedKeys
    (k', newMap', addUpdates) = foldl
        (\(k, em, upds) x -> (succ k, EnumMap.insert k x em, upds ++ [AddItem k x]))
        (initialK, newMap, []) newItems

-------------------------------------------------------------------------------
-- Converting Discrete Maps into Collections

mapToCollection :: forall k a.
                  (Enum k, Eq k, Eq a)
                => Discrete (EnumMap k a)
                -> SignalGen (Collection k (Discrete a))
mapToCollection mapD =
    genericMapToCollection EnumMap.empty $
        diffMaps (EnumMap.\\) EnumMap.intersection EnumMap.lookup EnumMap.toList

data MapCollEvent k a
    = MCNew k a
    | MCChange k a
    | MCRemove k

-- | Turns mapping of values into a collection of first-class FRP
-- values that are updated. If items are added to the EnumMap, then
-- they will be added to the Collection. Likewise, if they are removed
-- from the mapping, they will be removed from the collection. Keys
-- that are present in both but have new values will have their
-- Discrete value updated, and keys with values that are still present
-- will not have their Discrete values updated.
genericMapToCollection
    :: forall c k a. (Eq k)
    => c k a                                  -- ^ Empty map
    -> (c k a -> c k a -> [MapCollEvent k a]) -- ^ Function for diffing maps
    -> Discrete (c k a)                       -- ^ A discrete of maps
    -> SignalGen (Collection k (Discrete a))
genericMapToCollection emptyMap diffFn mapD = do
    m1 <- delayD emptyMap mapD
    let collDiffs :: Discrete [MapCollEvent k a]
        collDiffs = diffFn <$> m1 <*> mapD
    dispatchCollEvent . flattenE =<< preservesD collDiffs

-- | Given a pair of generic maps, compute a sequence of "MapCollEvent"s
-- which would transform the first into the second.
diffMaps
    :: (Eq a)
    => (c k a -> c k a -> c k a) -- ^ Difference
    -> (c k a -> c k a -> c k a) -- ^ Intersection
    -> (k -> c k a -> Maybe a)   -- ^ Lookup
    -> (c k a -> [(k, a)])       -- ^ To list
    -> c k a -> c k a -> [MapCollEvent k a]
diffMaps difference intersection mapLookup toList prevmap newmap = concat
    [ map (uncurry MCNew   ) newStuff
    , map (MCRemove . fst  ) removedStuff
    , map (uncurry MCChange) changedStuff
    ]
  where
    newStuff     = toList $ newmap `difference` prevmap
    removedStuff = toList $ prevmap `difference` newmap
    keptStuff    = toList $ newmap `intersection` prevmap
    changedStuff = mapMaybe justChanges keptStuff
    justChanges (k, v1) = case mapLookup k prevmap of
        Just v2 | v1 /= v2  -> Just (k, v1)
        _ -> Nothing

dispatchCollEvent
    :: (Eq k)
    => Event (MapCollEvent k a)
    -> SignalGen (Collection k (Discrete a))
dispatchCollEvent mapcollE = do
    let f (MCNew k a) = Just $
            AddItem k <$> discreteForKey k a mapcollE
        f (MCRemove k) = Just $ return $ RemoveItem k
        f (MCChange _ _) = Nothing
    updateEv <- generatorE $ justE (f <$> mapcollE)
    genericAccumCollection updateEv

discreteForKey :: Eq k => k -> a -> Event (MapCollEvent k a) -> SignalGen (Discrete a)
discreteForKey targetKey v0 mapcollE =
    stepperD v0 $ justE $ relevantValue <$> mapcollE
  where
    relevantValue collEvent = case collEvent of
        MCChange k v | k == targetKey -> Just v
        _ -> Nothing

-------------------------------------------------------------------------------

-- | Look for a key in a collection, and give its (potentially
-- nonexistant) value over time.
followCollectionKey :: forall k a. (Eq k)
                    => k
                    -> Collection k a
                    -> SignalGen (Discrete (Maybe a))
followCollectionKey k (Collection coll) = do
    collAsNow <- takeE 1 =<< preservesD coll
        :: SignalGen (Event ([(k, a)], Event (CollectionUpdate k a)))
    let existing :: Event (CollectionUpdate k a)
        existing = flattenE $ initialAdds . fst <$> collAsNow
        further :: Event (Event (CollectionUpdate k a))
        further  = snd <$> collAsNow
    further' <- switchD =<< stepperD mempty further
        :: SignalGen (Event (CollectionUpdate k a))
    accumMatchingItem (== k) (existing `mappend` further')

-- Turn the existing items into AddItems for our state accumulation
initialAdds :: [(k, a)] -> [CollectionUpdate k a]
initialAdds = map (uncurry AddItem)

-- Accumulate CollectionUpdates, and keep the newest value whose key
-- is True for the given function.
accumMatchingItem :: forall k a.
                  (k -> Bool)
                  -> Event (CollectionUpdate k a)
                  -> SignalGen (Discrete (Maybe a))
accumMatchingItem f updateE =
    stepperD Nothing $ justE (g <$> updateE)
  where
    g :: CollectionUpdate k a -> Maybe (Maybe a)
    g (AddItem k a)  | f k       = Just (Just a)
                     | otherwise = Nothing
    g (RemoveItem k) | f k       = Just Nothing
                     | otherwise = Nothing

-- | Extracts a 'Discrete' which represents the current state of
-- a collection.
collectionToDiscreteList :: Collection k a -> Discrete [(k, a)]
collectionToDiscreteList = fmap fst . unCollection

-- | Extracts a snapshot of the current values in a collection with
-- an 'Event' stream of further updates
openCollection :: Collection k a -> SignalGen ([(k,a)], Event (CollectionUpdate k a))
openCollection = snapshotD . unCollection
