{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Data.FixedSize.Class
Description : class definitions
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the class of fixed-size containers.

-}

module Data.FixedSize.Class
    ( FixedSize(..) 
    , (!)
    ) where

import GHC.TypeLits
import Data.MyPrelude

-- | The class of fixed-size containers.
--
class (Applicative f, Traversable f) => FixedSize (f :: * -> *) where

    -- | The type of indices.
    --
    type Index f :: *

    -- | The size of the container.
    --
    type Size f :: Nat

    -- | Gets the element at the specified index if the index is valid,
    --   otherwise 'Nothing'.
    --
    (!?) :: f a -> Index f -> Maybe a

    -- | Generates a container by applying the given function to each index.
    --
    generate :: (Index f -> a) -> f a

-- | Gets the element at the specified index, throws an exception
--   if the index is invalid.
--
(!) :: FixedSize f => f a -> Index f -> a
x ! i = fromMaybe (error "Data.FixedSize.Class.!: invalid index") (x !? i)
