{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

-- | Collection signals with incremental updates.
--
-- The interface exposed by this module uses "Maplike" extensively. This
-- allows it to be used in the context of a wider variety of key
-- constrints, at the cost of requiring the caller to provide a "Proxy".
-- This is pretty inconvenient, so it's recommended that people use one of
-- the specialised modules:
--
-- - FRP.Euphoria.Collection.Enum
-- - FRP.Euphoria.Collection.Hashable
--
module FRP.Euphoria.Internal.GenericCollection
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

import Prelude hiding (lookup)

import Control.Monad (join)
import Control.DeepSeq
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy(..))

import FRP.Euphoria.Event
import qualified FRP.Euphoria.Internal.Maplike as M

import GHC.Generics

-- | Represents an incremental change to a collection of items.
data CollectionUpdate k a
    = AddItem k a
    | RemoveItem k
    deriving (Functor, Eq, Show, Foldable, Traversable, Generic)

instance (NFData k, NFData v) => NFData (CollectionUpdate k v)

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
mapCollection :: MonadSignalGen m => (a -> b) -> Collection k a -> m (Collection k b)
mapCollection = mapCollectionWithKey . const

-- | @mapCollectionM p fn = mapCollection fn >=> sequenceCollection p@
mapCollectionM
    :: (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k) -> (a -> SignalGen b) -> Collection k a -> m (Collection k b)
mapCollectionM p fn coll = do
    updatesE0 <- collectionToUpdates coll
    updatesE1 <- generatorE $ traverse fn <$> updatesE0
    updatesE2 <- memoE updatesE1
    accumCollection p updatesE2

-- | A version of 'mapCollection' which provides access to the key
mapCollectionWithKey
    :: MonadSignalGen m
    => (k -> a -> b) -> Collection k a -> m (Collection k b)
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

filterCollection
    :: (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k) -> (a -> Bool) -> Collection k a -> m (Collection k a)
filterCollection p =
    filterCollectionWithKey p . const

filterCollectionWithKey
    :: (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k) -> (k -> a -> Bool) -> Collection k a -> m (Collection k a)
filterCollectionWithKey p f aC =
     justCollection p =<< mapCollectionWithKey f' aC
  where
    f' k v
        | f k v = Just v
        | otherwise = Nothing

justCollection
    :: forall m c k a. (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k) -> Collection k (Maybe a) -> m (Collection k a)
-- Inefficient, quick-hack implementation
justCollection p c = do
    upds <- collectionToUpdates c
    let f :: CollectionUpdate k (Maybe a) -> c k () -> (c k (), Maybe (CollectionUpdate k a))
        f (AddItem k Nothing) m = (M.insert k () m, Nothing)
        f (AddItem k (Just a)) m = (m, Just (AddItem k a))
        f (RemoveItem k) m = case M.lookup k m of
            Just () -> (M.delete k m, Nothing)
            Nothing -> (m, Just (RemoveItem k))
    upds' <- scanAccumE M.empty (f <$> upds)
    accumCollection p =<< memoE (justE upds')

-- | Create an 'Event' stream of all updates from a collection, including
-- the items currently in it.
collectionToUpdates
    :: forall m k a. MonadSignalGen m
    => Collection k a
    -> m (Event (CollectionUpdate k a))
collectionToUpdates aC = do
    (cur,updateE) <- openCollection aC
    initE  <- onCreation (map (uncurry AddItem) cur)
    initE' <- memoE $ flattenE initE
    return (updateE `mappend` initE')

sequenceCollection
    :: (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k) -> Collection k (SignalGen a)
    -> m (Collection k a)
sequenceCollection p coll =
    mapCollectionM p id coll

-- | A collection whose items are created by an event, and removed by
-- another event.
simpleCollection
    :: (M.Maplike c k, Enum k, MonadSignalGen m)
    => Proxy (c k)
    -> k                    -- ^ The initial value for the unique keys.
                            -- 'succ' will be used to get further keys.
    -> Event (a, Event ())  -- ^ An Event that introduces a new item and
                            -- its subsequent removal Event. The item will
                            -- be removed from the collection when the
                            -- Event () fires.
    -> m (Collection k a)
simpleCollection p initialK evs =
    simpleCollectionUpdates p initialK evs >>= accumCollection p

simpleCollectionUpdates
    :: forall m c k a. (M.Maplike c k, Enum k, MonadSignalGen m)
    => Proxy (c k)
    -> k
    -> Event (a, Event ())
    -> m (Event (CollectionUpdate k a))
simpleCollectionUpdates _ initialK evs = do
    let addKey (a, ev) k = (succ k, (k, a, ev))
    newEvents <- scanAccumE initialK (addKey <$> evs)
    let addItem (k, _a, ev) = M.insert k ev
    rec
        removalEvent' <- delayE removalEvent
        removalEvents <- accumD (M.empty :: c k (Event ()))
            ((addItem <$> newEvents) `mappend` (M.delete <$> removalEvent'))
        removalEvent <- switchD $ M.foldrWithKey
            (\k ev ev' -> (k <$ ev) `mappend` ev') mempty <$> removalEvents
    let updateAddItem :: (k, a, Event ()) -> CollectionUpdate k a
        updateAddItem (k, a, _) = AddItem k a
    memoE $ (updateAddItem <$> newEvents) `mappend` (RemoveItem <$> removalEvent)

-- Adds the necessary state for holding the existing [(k, a)] and creating
-- the unique Event stream for each change of the collection.
accumCollection
    :: forall m c k a. (M.Maplike c k, MonadSignalGen m)
    => Proxy (c k)
    -> Event (CollectionUpdate k a)
    -> m (Collection k a)
accumCollection _ ev = do
    let toMapOp :: CollectionUpdate k a -> c k a -> c k a
        toMapOp (AddItem k a) = M.insert k a
        toMapOp (RemoveItem k) = M.delete k
    mapping <- accumD M.empty (toMapOp <$> ev)
    listD <- memoD $ M.toList <$> mapping
    makeCollection listD ev

-- | The primitive interface for creating a 'Collection'. The two
-- arguments must be coherent, i.e. the value of the discrete at
-- time /t+1/ should be obtained by applying the updates
-- at /t+1/ to the value of the discrete at /t/. This invariant
-- is not checked.
makeCollection
    :: MonadSignalGen m
    => Discrete [(k, a)]
    -> Event (CollectionUpdate k a)
    -> m (Collection k a)
makeCollection listD updE = Collection <$> generatorD (gen <$> listD)
    where
        gen list = do
            updE' <- dropStepE updE
            return (list, updE')

-- | Prints add/remove diagnostics for a Collection. Useful for debugging
watchCollection :: (Show k, Show a, MonadSignalGen m)
                => Collection k a -> m (Event (IO ()))
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
    :: forall m c k a. (M.Maplike c k, Enum k, Eq a, MonadSignalGen m)
    => Proxy (c k)
    -> k
    -> Discrete [a]
    -> m (Collection k a)
collectionFromDiscreteList p initialK valsD = do
    valsE <- preservesD valsD
    evs <- scanAccumE (initialK, M.empty :: c k a) (stepListCollState <$> valsE)
    accumCollection p (flattenE evs)

-- This could obviously be implemented more efficiently.
stepListCollState
    :: (M.Maplike c k, Enum k, Eq a)
    => [a]
    -> (k, c k a)
    -> ((k, c k a), [CollectionUpdate k a])
stepListCollState xs (initialK, existingMap) =
    ((k', newMap'), removeUpdates ++ addUpdates)
  where
    keyvals = M.toList existingMap
    newItems = xs List.\\ map snd keyvals
    removedKeys = map fst $ List.deleteFirstsBy
        (\(_, x) (_, y) -> x == y)
        keyvals
        (map (\x -> (initialK, x)) xs)
    (newMap, removeUpdates) = foldl
        (\(em, upds) k -> (M.delete k em, upds ++ [RemoveItem k]))
        (existingMap, []) removedKeys
    (k', newMap', addUpdates) = foldl
        (\(k, em, upds) x -> (succ k, M.insert k x em, upds ++ [AddItem k x]))
        (initialK, newMap, []) newItems

-------------------------------------------------------------------------------
-- Converting Discrete Maps into Collections

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
mapToCollection
    :: forall c m k a. (Eq k, Eq a, M.Maplike c k, MonadSignalGen m)
    => Discrete (c k a)
    -> m (Collection k (Discrete a))
mapToCollection mapD = do
    m0 <- delayD M.empty mapD
    let diffsD = diffMaps <$> m0 <*> mapD
    diffsE <- flattenE <$> preservesD diffsD
    dispatchCollEvent (Proxy :: Proxy (c k)) diffsE

-- | Given a pair of generic maps, compute a sequence of "MapCollEvent"s
-- which would transform the first into the second.
diffMaps
    :: (Eq a, M.Maplike c k)
    => c k a
    -> c k a
    -> [MapCollEvent k a]
diffMaps prevmap newmap = concat
    [ map (uncurry MCNew   ) newStuff
    , map (MCRemove . fst  ) removedStuff
    , map (uncurry MCChange) changedStuff
    ]
  where
    newStuff     = M.toList $ newmap `M.difference` prevmap
    removedStuff = M.toList $ prevmap `M.difference` newmap
    keptStuff    = M.toList $ newmap `M.intersection` prevmap
    changedStuff = mapMaybe justChanges keptStuff
    justChanges (k, v1) = case M.lookup k prevmap of
        Just v2 | v1 /= v2  -> Just (k, v1)
        _ -> Nothing

dispatchCollEvent
    :: (Eq k, M.Maplike c k, MonadSignalGen m)
    => Proxy (c k)
    -> Event (MapCollEvent k a)
    -> m (Collection k (Discrete a))
dispatchCollEvent p mapcollE = do
    let f (MCNew k a) = Just $
            AddItem k <$> discreteForKey k a mapcollE
        f (MCRemove k) = Just $ return $ RemoveItem k
        f (MCChange _ _) = Nothing
    updateEv <- generatorE $ justE (f <$> mapcollE)
    accumCollection p updateEv

discreteForKey :: (Eq k, MonadSignalGen m) => k -> a -> Event (MapCollEvent k a) -> m (Discrete a)
discreteForKey targetKey v0 mapcollE =
    stepperD v0 $ justE $ relevantValue <$> mapcollE
  where
    relevantValue collEvent = case collEvent of
        MCChange k v | k == targetKey -> Just v
        _ -> Nothing

-------------------------------------------------------------------------------

-- | Look for a key in a collection, and give its (potentially
-- nonexistant) value over time.
followCollectionKey :: forall m k a. (Eq k, MonadSignalGen m)
                    => k
                    -> Collection k a
                    -> m (Discrete (Maybe a))
followCollectionKey k (Collection coll) = do
    collAsNow <- takeE 1 =<< preservesD coll
        :: m (Event ([(k, a)], Event (CollectionUpdate k a)))
    let existing :: Event (CollectionUpdate k a)
        existing = flattenE $ initialAdds . fst <$> collAsNow
        further :: Event (Event (CollectionUpdate k a))
        further  = snd <$> collAsNow
    further' <- switchD =<< stepperD mempty further
        :: m (Event (CollectionUpdate k a))
    accumMatchingItem (== k) (existing `mappend` further')

-- Turn the existing items into AddItems for our state accumulation
initialAdds :: [(k, a)] -> [CollectionUpdate k a]
initialAdds = map (uncurry AddItem)

-- Accumulate CollectionUpdates, and keep the newest value whose key
-- is True for the given function.
accumMatchingItem :: forall m k a. MonadSignalGen m =>
                  (k -> Bool)
                  -> Event (CollectionUpdate k a)
                  -> m (Discrete (Maybe a))
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
openCollection :: MonadSignalGen m => Collection k a -> m ([(k,a)], Event (CollectionUpdate k a))
openCollection = snapshotD . unCollection
