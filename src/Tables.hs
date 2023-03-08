module Tables where

import Prelude hiding (lookup)

-- START HERE AFTER reaching the pointer in Datatypes.hs

newtype Table k v = Table [(k, v)]
  deriving (Show)

-- In the following, we first reimplement the functions
-- from the slides, but with the @newtype@-based version
-- of the 'Table' type.

-- Task Tables-1.
--
-- Re-implement 'empty'.

empty :: Table k v
empty = (Table [])

-- Task Tables-2.
--
-- Re-implement 'insert'.

insert :: k -> v -> Table k v -> Table k v
insert k' v' (Table t) = Table ((k', v') : t)

-- Task Tables-3.
--
-- Re-implement 'delete'.

delete :: Eq k => k -> Table k v -> Table k v
delete _ (Table []) = Table []
delete k' t@(Table (x : xs))
  | k' == (fst x) = delete k' (Table xs)
  | otherwise = insert (fst x) (snd x) (delete k' (Table xs))

-- Task Tables-4.
--
-- Re-implement 'lookup'.

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table []) = Nothing
lookup k' t@(Table (x : xs))
  | k' == (fst x) = Just (snd x)
  | otherwise = lookup k' (Table xs)

-- Task Tables-5.
--
-- Implement a map function on the table values.

mapValues :: (v1 -> v2) -> Table k v1 -> Table k v2
mapValues _ (Table []) = Table []
mapValues f (Table ((k', v') : xs)) = insert k' (f v') (mapValues f (Table xs))

-- Task Tables-6.
--
-- Implement a map function on the table keys.
--
-- Tricky additional question:
-- Can you identify a potential problem with
-- this function?

mapKeys :: (k1 -> k2) -> Table k1 v -> Table k2 v
mapKeys _ (Table []) = Table []
mapKeys f (Table ((k', v') : xs)) = insert (f k') v' (mapKeys f (Table xs))

-- Task Tables-7.
--
-- Implement a more general table update function.
-- The function 'alter' takes a function and a key.

alter :: Eq k => (Maybe v -> Maybe v) -> k -> Table k v -> Table k v
alter _ _ (Table []) = Table []
alter f key (Table ((k', v') : xs))
  | key == k' = insert k' (f v') (alter f key (Table xs))
  | otherwise = alter f key (Table xs)

-- Task Tables-8.
--
-- Add an export list to the module, exporting
-- all the functions, and the 'Table' type, but
-- no constructors. The syntax
--
--   Table()
--
-- can be used in the export list to export a
-- datatype or newtype without any of its
-- constructors.

-- GO TO Transactions.hs
