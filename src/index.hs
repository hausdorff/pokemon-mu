module Index (
      Index(..)
    , IndexedSet
    ) where

import qualified Data.List as L
import qualified Data.Map as M

class Index i where
    makeKey :: Ord k => i k v -> v -> k
    empty :: (v -> k) -> (v -> v -> v) -> i k v
    singleton :: (v -> k) -> (v -> v -> v) -> v -> i k v
    insert :: (Ord k) => v -> i k v -> i k v
    lookup :: (Ord k) => v -> i k v -> Maybe v
    lookupKey :: (Ord k) => k -> i k v -> Maybe v
    toList :: i k v -> [v]
    fromFoldable :: (Foldable t, Ord k) => (v -> k) -> (v -> v -> v) -> t v -> i k v

data IndexedSet k v = IndexedSet (v -> k) (v -> v -> v) (M.Map k v)
instance Index IndexedSet where
    makeKey (IndexedSet indexer _ _) v = indexer v
    empty indexer merger = IndexedSet indexer merger M.empty
    singleton indexer merger v = IndexedSet indexer merger (M.singleton k v)
        where k = indexer v
    insert v is@(IndexedSet indexer merger m) = case (Index.lookup v is) of
        Nothing     -> (IndexedSet indexer merger (M.insert k v m))
        Just v' -> (IndexedSet indexer merger (M.insert k newValue m))
            where newValue = merger v v'
        where k      = makeKey is v
    lookup v is@(IndexedSet _ _ m) = M.lookup k m
        where k = makeKey is v
    lookupKey k (IndexedSet _ _ m) = M.lookup k m
    toList (IndexedSet _ _ m) = M.foldl (\listAcc e -> e:listAcc) [] m
    fromFoldable indexer merger t = foldl (\set v -> insert v set) (empty indexer merger) t
