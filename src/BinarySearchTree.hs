module BinarySearchTree where

import qualified Data.List
import qualified Data.Set
-- You might want to use some externals. Better use qualified import
-- so there won't be any clash
-- for example instead of "sort" (from Data.List) use "Data.List.sort"

-- !! DO NOT CHANGE BSTree data type and type signatures of functions

-- | Binary search tree as described at wikipedia:
--  https://en.wikipedia.org/wiki/Binary_search_tree
data BSTree a = Node a (BSTree a) (BSTree a)
              | Nil
              deriving (Show, Read, Eq)

value :: BSTree a -> a
value Nil = error "Nil does not have a value"
value (Node x _ _) = x

left :: BSTree a -> BSTree a
left Nil = Nil
left (Node _ l _) = l

right :: BSTree a -> BSTree a
right Nil = Nil
right (Node _ _ r) = r

-- | Check whether is @BSTree@ valid (i.e., does not violate any rule)
isValid :: Ord a => BSTree a -> Bool
isValid = isSorted . toList
    where isSorted xs = and $ zipWith (<=) xs (tail xs)

-- | Check whether is @BSTree@ is leaf
isLeaf :: Ord a => BSTree a -> Bool
isLeaf (Node _ Nil Nil) = True
isLeaf _ = False

-- | Count all nodes in @BSTree@
size :: BSTree a -> Integer
size Nil = 0
size (Node _ l r) = 1 + (size l) + (size r)

-- | Height of @BSTree@ (height of @Nil@ is 0)
height :: BSTree a -> Integer
height Nil = 0
height (Node _ l r) = 1 + max (height l) (height r)

-- | Minimal height in the @BSTree@ (height of @Nil@ is 0)
minHeight :: BSTree a -> Integer
minHeight Nil = 0
minHeight (Node _ l r) = 1 + min (height l) (height r)

-- | Check if given element is in the @BSTree@
contains :: Ord a => BSTree a -> a -> Bool
contains Nil _ = False
contains (Node x l r) y
    | x > y = contains l y
    | x < y = contains r y
    | otherwise = x == y

-- | Create new tree with given element inserted
insert :: Ord a => BSTree a -> a -> BSTree a
insert Nil x = Node x Nil Nil
insert (Node x l r) y
    | x > y = Node x (insert l y) r
    | x < y = Node x l (insert r y)
    | otherwise = Node x l r

-- | Create new tree with given element deleted (min element in the right subtree strategy)
delete :: Ord a => BSTree a -> a -> BSTree a
delete Nil _ = Nil
delete (Node x l r) y
    | x > y = Node x (delete l y) r
    | x < y = Node x l (delete r y)
    | l == Nil = r
    | r == Nil = l
    | otherwise = Node newRoot l (delete r newRoot)
    where newRoot = minValue r

-- | Finds minimal value in given tree. Fails for empty tree.
minValue :: Ord a => BSTree a -> a
minValue (Node a Nil _) = a
minValue (Node _ l _) = minValue l
minValue _ = error "Nil does not have a value"

-- | Convert @BSTree@ to list (will be in ascending order if tree is valid)
toList :: BSTree a -> [a]
toList Nil = []
toList (Node a l r) = (toList l) ++ [a] ++ (toList r)

-- | Build new @BSTree@ from arbitrary list with use of median (left if even)
fromList :: Ord a => [a] -> BSTree a
fromList [] = Nil
fromList xs = makeNode $ half $ Data.List.sort $ unique xs
    where makeNode (l, r) = Node (last l) (fromList $ init l) (fromList r)

-- | Splits the given list into two (almost) equal sub-lists
half :: [a] -> ([a], [a])
half xs = Data.List.splitAt index xs
    where index = (length xs + 1) `div` 2

-- | Removes duplicites from list in `O(n log n)`
unique :: Ord a => [a] -> [a]
unique = Data.Set.toList . Data.Set.fromList
