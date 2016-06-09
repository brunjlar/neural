{-# OPTIONS_HADDOCK show-extensions #-}
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
    ) where

import Control.Monad.Codensity (lowerCodensity)
import Data.MyPrelude
import Pipes

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
