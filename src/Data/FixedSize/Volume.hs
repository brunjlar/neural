{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Data.FixedSize.Volume
Description : fixed-size volumes
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixed-size /volumes/ and some basic typeclass instances and operations for them.
A 'Volume' is a 'Matrix' with 'Vector' entries, i.e. a three-dimensional array.
-}

module Data.FixedSize.Volume
    ( Volume(..)
    , toVolume
    , slice
    , fromMatrix
    ) where

import Data.MyPrelude
import Data.FixedSize.Class
import Data.FixedSize.Matrix
import Data.FixedSize.Vector
import GHC.TypeLits

-- | @'Volume' m n d a@ is the type of /volumes/ with @m@ rows, @n@ columns, depth @d@ and entries of type @a@.
--
newtype Volume (m :: Nat) (n :: Nat) (d :: Nat) a = Volume (Matrix m n (Vector d a))
    deriving (Eq, Show, Functor, Foldable, Traversable, NFData)

-- | Converts a 'Matrix' of 'Vector's to the equivalent 'Volume'.
--
toVolume :: Matrix m n (Vector d a) -> Volume m n d a
toVolume = Volume

instance (KnownNat m, KnownNat n, KnownNat d) => Applicative (Volume m n d) where

    pure x = Volume $ pure (pure x)

    Volume fs <*> Volume xs = Volume $ (<*>) <$> fs <*> xs

instance (KnownNat m, KnownNat n, KnownNat d) => FixedSize (Volume m n d) where

    type Index (Volume m n d) = (Int, Int, Int)

    type Size (Volume m n d) = m * n * d

    Volume m !? (i, j, k) = m !? (i, j) >>= (!? k)

    generate f = Volume $ generate (\(i, j) -> generate (\k -> f (i, j, k)))

-- | @'slice' v i@ gives the matrix "at depth @i@" of volume @v@ (or 'Nothing' if @i@ is invalid).
--
-- >>> :set -XDataKinds
-- >>> slice (pure True :: Volume 2 2 3 Bool) 2
-- Just (Matrix [[True,True],[True,True]])
--
-- >>> slice (pure True :: Volume 2 2 3 Bool) 3
-- Nothing
--
slice :: KnownNat d => Volume m n d a -> Int -> Maybe (Matrix m n a)
slice (Volume m) i = sequenceA $ (!? i) <$> m

-- | Converts a 'Matrix' into a 'Volume' of depth one.
--
-- >>> fromMatrix (pure 0) :: Volume 2 2 1 Int
-- Volume (Matrix [[[0],[0]],[[0],[0]]])
--
fromMatrix :: Matrix m n a -> Volume m n 1 a
fromMatrix = Volume . fmap pure
