{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Utils.Pipes
Description : pipe utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides various utilities for working with pipes.
-}

module Data.Utils.Pipes
    ( chunks
    , fromFile
    , toWord8
    , MonadSafe
    , runSafeP
    , runSafeT
    , ByteString
    , Word8
    , indicesP
    ) where

import           Control.Monad.Codensity (lowerCodensity)
import           Data.ByteString         (ByteString, unpack)
import           Data.MyPrelude
import           Data.Word               (Word8)
import           Data.Utils.List         (pairs)
import           Pipes
import           Pipes.ByteString        (fromHandle)
import           Pipes.Safe
import qualified Pipes.Safe.Prelude      as P

-- | Collects upstream data in chunks of a specified size and then passes those chunks downstram.
--
-- >>> import qualified Pipes.Prelude as P
-- >>> runEffect $ each [1 .. 10 :: Int] >-> chunks 3 >-> P.mapM_ print
-- [1,2,3]
-- [4,5,6]
-- [7,8,9]
--
-- >>> runEffect $ each [1 .. 30000 :: Int] >-> chunks 10000 >-> P.mapM_ (print . sum)
-- 50005000
-- 150005000
-- 250005000
--
chunks :: Monad m => Int -> Pipe a [a] m ()
chunks n = forever $ lowerCodensity (replicateM n (lift await)) >>= yield

-- | Safely produces 'ByteString's from a file.
--
fromFile :: MonadSafe m => FilePath -> Producer' ByteString m ()
fromFile f = P.withFile f ReadMode fromHandle

-- | Converts a stream of 'ByteString's into a stream of 'Word8's.
--
toWord8 :: Monad m => Pipe ByteString Word8 m ()
toWord8 = forever $ await >>= each . unpack

-- | Passes upstream elements with the specified indices downstream.
--   Indices will first be sorted, duplicate indices are allowed, negative indices will be ignored.
--
-- >>> import qualified Pipes.Prelude as P
-- >>> P.toList (each [0 .. 10 :: Int] >-> indicesP [2,2,2,0,0,1,1,-3])
-- [0,0,1,1,2,2,2]
--
indicesP :: Monad m => [Int] -> Pipe a a m ()
indicesP = indicesP' . offsets where

    offsets xs = [y - x | (x, y) <- pairs $ 0 : sort [x | x <- xs, x >= 0]]

    indicesP' []       = return ()
    indicesP' (x : xs) = do
        y <- await
        indicesP'' x y xs

    indicesP'' 0 y []       = yield y
    indicesP'' 0 y (0 : xs) = yield y >> indicesP'' 0 y xs
    indicesP'' 0 y (n : xs) = yield y >> indicesP' (pred n : xs)
    indicesP'' n _ xs       = indicesP' $ pred n : xs
