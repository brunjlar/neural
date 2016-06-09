{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Utils.List
Description : list utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides various utilities for working with lists.
-}

module Data.Utils.List
    ( splitLast
    , pick
    , distribute
    , pad
    , ListEditorT
    , editListT
    , editT
    , tryLeftT
    , tryRightT
    , focusT
    , ListEditor
    , editList
    , pairs
    , indexOf
    , safeHead
    ) where

import qualified Control.Monad.Identity as I
import qualified Control.Monad.State    as S
import           Data.MyPrelude         (runIdentity)
import           Data.Utils.Traversable (fromList)

-- | Splits off the last element of a non-empty list.
--
-- >>> splitLast [1, 2, 3]
-- Just ([1,2],3)
--
-- >>> splitLast []
-- Nothing
--
splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs@(_ : _)) =
    let Just (ys, y) = splitLast xs
    in  Just (x : ys, y)

-- | Given a valid index, returns the list element at the index and the remaining elements.
--
-- >>> pick 1 [1,2,3,4]
-- (2,[1,3,4])
--
pick :: Int -> [a] -> (a, [a])
pick n xs = let (ys, z : zs) = splitAt n xs in (z, ys ++ zs)

-- | Distributes the elements of a list as uniformly as possible amongst a specified number of groups.
--
-- >>> distribute 3 [1,2,3,4,5]
-- [[3],[4,1],[5,2]]
--
distribute :: Int -> [a] -> [[a]]
distribute n = go (replicate n []) where

    go :: [[a]] -> [a] -> [[a]]
    go acc []                = acc
    go (acc : accs) (x : xs) = go (accs ++ [x : acc]) xs
    go _ _                   = error "need something to distribute to"

-- | Pads a litst with a provided element on the left.
--
-- >>> pad 4 'x' "oo"
-- "xxoo"
--
pad :: Int -> a -> [a] -> [a]
pad l x xs = replicate (l - length xs) x ++ xs

type LZ a = ([a], [a])

lz :: [a] -> LZ a
lz xs = ([], xs)

lzToList :: LZ a -> [a]
lzToList (xs, ys) = reverse xs ++ ys

lzEdit :: [a] -> LZ a -> LZ a
lzEdit ys (xs, _) = (xs, ys)

lzLeft :: LZ a -> Maybe (LZ a)
lzLeft ([]    , _ ) = Nothing
lzLeft (x : xs, ys) = Just (xs, x : ys)

lzRight :: LZ a -> Maybe (LZ a)
lzRight (_ , []    ) = Nothing
lzRight (xs, y : ys) = Just (y : xs, ys)

lzFocus :: LZ a -> [a]
lzFocus = snd

-- | @'ListEditorT' a m@ is a monad transformer for editting lists of type @[a]@.
--
newtype ListEditorT a m b = ListEditorT (S.StateT (LZ a) m b) 
    deriving (Functor, Applicative, Monad, S.MonadState (LZ a))

-- | Runs the editor.
-- 
editListT :: Monad m => ListEditorT a m () -> [a] -> m [a]
editListT (ListEditorT e) xs = lzToList <$> S.execStateT e (lz xs)

-- | Replaces the list at the "cursor" with the provided list.
--
editT :: Monad m => [a] -> ListEditorT a m ()
editT = ListEditorT . S.modify . lzEdit

tryT :: Monad m => (LZ a -> Maybe (LZ a)) -> ListEditorT a m Bool
tryT f = do
    z <- S.get
    case f z of
        Nothing -> return False
        Just z' -> S.put z' >> return True

-- | Tries to move the "cursor" to the left.
--
tryLeftT :: Monad m => ListEditorT a m Bool
tryLeftT = tryT lzLeft

-- | Tries to move the "cursor" to the right.
--
tryRightT :: Monad m => ListEditorT a m Bool
tryRightT = tryT lzRight

-- | Gets the list under the "cursor".
--
focusT :: Monad m => ListEditorT a m [a]
focusT = lzFocus <$> S.get

-- | Monad for pure list editting.
--
type ListEditor a = ListEditorT a I.Identity

-- | Runs the pure editor.
--
-- >>> editList (do _ <- tryRightT; editT [3,2]) [1,2,3]
-- [1,3,2]
--
editList :: ListEditor a () -> [a] -> [a]
editList e xs = I.runIdentity $ editListT e xs 

-- | Gets all pairs of adjacent list elements.
--
-- >>> pairs "Haskell"
-- [('H','a'),('a','s'),('s','k'),('k','e'),('e','l'),('l','l')]
--
pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

-- | Gets the first index of the provided element in the list or 'Nothing' if it is not in the list.
--
-- >>> indexOf "Haskell" 'l'
-- Just 5
--
-- >>> indexOf "Haskell" 'y'
-- Nothing
--
indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf [] _ = Nothing
indexOf (x : xs) y
    | x == y    = Just 0
    | otherwise = succ <$> indexOf xs y

-- | Returns the head of a non-empty list or 'Nothing' for the empty list.
--
-- >>> safeHead "Haskell"
-- Just 'H'
--
-- >>> safeHead ""
-- Nothing
--
safeHead :: [a] -> Maybe a
safeHead = (runIdentity <$>) . fromList
