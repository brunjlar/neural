{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Utils.Traversable
Description : utilities for traversables
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module contains utility functions related to the 'Traversable' typeclass.
-}

module Data.Utils.Traversable
    ( fromList
    ) where

import Data.Utils.Stack

-- | Tries to create a traversable (which must also be applicative) from a list.
--   If the list contains too few elements, 'Nothing' is returned,
--
-- >>> import Data.MyPrelude
-- >>> fromList [1, 2, 3] :: Maybe (Identity Int)
-- Just (Identity 1)
--
-- >>> fromList [] :: Maybe (Identity Char)
-- Nothing
--
fromList :: (Applicative t, Traversable t) => [a] -> Maybe (t a)
fromList xs = sequenceA $ evalStack (sequenceA $ pure pop) xs
