{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Utils.List
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
    ) where

import qualified Control.Monad.Identity as I
import qualified Control.Monad.State    as S

splitLast :: [a] -> Maybe ([a], a)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x : xs@(_ : _)) =
    let Just (ys, y) = splitLast xs
    in  Just (x : ys, y)

pick :: Int -> [a] -> (a, [a])
pick n xs = let (ys, z : zs) = splitAt n xs in (z, ys ++ zs)

distribute :: Int -> [a] -> [[a]]
distribute n = go (replicate n []) where

    go :: [[a]] -> [a] -> [[a]]
    go acc []                = acc
    go (acc : accs) (x : xs) = go (accs ++ [x : acc]) xs
    go _ _                   = error "need something to distribute to"

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

newtype ListEditorT a m b = ListEditorT (S.StateT (LZ a) m b) 
    deriving (Functor, Applicative, Monad, S.MonadState (LZ a))

editListT :: Monad m => ListEditorT a m () -> [a] -> m [a]
editListT (ListEditorT e) xs = lzToList <$> S.execStateT e (lz xs)

editT :: Monad m => [a] -> ListEditorT a m ()
editT = ListEditorT . S.modify . lzEdit

tryT :: Monad m => (LZ a -> Maybe (LZ a)) -> ListEditorT a m Bool
tryT f = do
    z <- S.get
    case f z of
        Nothing -> return False
        Just z' -> S.put z' >> return True

tryLeftT :: Monad m => ListEditorT a m Bool
tryLeftT = tryT lzLeft

tryRightT :: Monad m => ListEditorT a m Bool
tryRightT = tryT lzRight

focusT :: Monad m => ListEditorT a m [a]
focusT = lzFocus <$> S.get

type ListEditor a = ListEditorT a I.Identity

editList :: ListEditor a () -> [a] -> [a]
editList e xs = I.runIdentity $ editListT e xs 

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

indexOf :: Eq a => [a] -> a -> Maybe Int
indexOf [] _ = Nothing
indexOf (x : xs) y
    | x == y    = Just 0
    | otherwise = succ <$> indexOf xs y
