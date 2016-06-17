{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Data.Utils.Cache
Description : caching
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines simple size-limitted caches and utilities for working with them.
-}

module Data.Utils.Cache
    ( Cache
    , newCache
    , retrieveC
    ) where

import qualified Data.Map       as M
import           Data.MyPrelude
import qualified Data.Set       as S

-- | A @'Cache' m k v@ is a cache with keys of type @k@ and values of type @v@.
--   If an uncached key is requested, its value is retrieved using effects in monad @m@.
--
data Cache m k v = Cache
    { cMap      :: M.Map k v
    , cQueue    :: [k]
    , cGet      :: [k] -> m [v]
    , cCapacity :: Int
    }

-- | Creates a new cache with the given retrieval function and maximal cache capacity.
--
newCache :: ([k] -> m [v]) -> Int -> Cache m k v
newCache g c = Cache
    { cMap      = M.empty
    , cQueue    = []
    , cGet      = g
    , cCapacity = max c 0
    }

instance (Show k, Show v) => Show (Cache m k v) where

    show c = show (cMap c, cQueue c)

-- | Retrieves the values for the specified keys from the cache.
--   Uncached values will be retrieved and cached while respecting the maximal cache capacity
--   (old values will be dropped first).
--
retrieveC :: forall m k v. (Monad m, Ord k) => Cache m k v -> [k] -> m ([v], Cache m k v)
retrieveC c xs = do
    let (m, xs') = f
        (ys, zs) = splitAt (length xs') $ cQueue c
        n        = foldl' (flip M.delete) (cMap c) ys
        xs''     = toList xs'
    vs <- cGet c xs''
    let xvs = zip xs'' vs 
        l   = max (cCapacity c) $ length ys
        m'  = foldl' (\m'' (x, v) -> M.insert x v m'') m xvs
        n'  = foldl' (\n'' (x, v) -> M.insert x v n'') n $ take l xvs
        zs' = zs ++ take l xs''
        c'  = c { cMap = n', cQueue = zs' }
        vs' = (m' M.!) <$> xs
    return (vs', c')

  where

    f :: (M.Map k v, S.Set k)
    f = foldl' f' (M.empty, S.empty) xs where
        f' (m, xs') x = case M.lookup x (cMap c) of
            Just y  -> (M.insert x y m,            xs')
            Nothing -> (             m, S.insert x xs')
