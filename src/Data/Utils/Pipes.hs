{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}

{-|
Module      : Data.Utils.Pipes
Description : list utilities
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
    ) where

import           Control.Monad.Codensity (lowerCodensity)
import           Data.ByteString         (ByteString, unpack)
import           Data.MyPrelude
import           Data.Word               (Word8)
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
