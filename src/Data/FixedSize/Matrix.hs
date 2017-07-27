{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.FixedSize.Matrix
Description : fixed-size matrices
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixed-size /matrices/ and some basic typeclass instances and operations for them.
-}

module Data.FixedSize.Matrix
    ( Matrix(..)
    , (<%%>)
    , row
    , column
    , transpose
    ) where

import Data.MyPrelude
import Data.FixedSize.Class
import Data.FixedSize.Vector
import GHC.TypeLits

-- | @'Matrix' m n a@ is the type of /matrices/ with @m@ rows, @n@ columns and entries of type @a@.
--
newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a))
    deriving (Eq, Show, Functor, Foldable, Traversable, NFData)

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

instance (KnownNat m, KnownNat n) => FixedSize (Matrix m n) where

    type Index (Matrix m n) = (Int, Int)

    type Size (Matrix m n) = m * n

    m !? (i, j) = row m i >>= (!? j)

    generate f = Matrix $ generate (\i -> generate (\j -> f (i, j)))

-- | Multiplication of a /matrix/ by a (column-)/vector/.
--
-- >>> :set -XDataKinds
-- >>> (pure 1 :: Matrix 1 2 Int) <%%> cons 1 (cons 2 nil)
-- [3]
--
(<%%>) :: Num a => Matrix m n a -> Vector n a -> Vector m a
Matrix rows <%%> v = (v <%>) <$> rows

-- | Gives the matrix row with the specified index (starting at zero) if the index is valid,
--   otherwise 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> row (pure 42 :: Matrix 2 4 Int) 0
-- Just [42,42,42,42]
--
-- >>> row (pure 42 :: Matrix 2 4 Int) 2
-- Nothing
--
row :: KnownNat m => Matrix m n a -> Int -> Maybe (Vector n a)
row (Matrix rows) = (rows !?)

-- | Gives the matrix column with the specified index (starting at zero) if the index is valid,
--   otherwise 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> column (pure 42 :: Matrix 2 4 Int) 3
-- Just [42,42]
--
-- >>> column (pure 42 :: Matrix 2 4 Int) 4
-- Nothing
--
column :: KnownNat n => Matrix m n a -> Int -> Maybe (Vector m a)
column (Matrix rows) j = sequenceA $ (!? j) <$> rows

-- | Transposes a matrix.
--
-- >>> transpose (Matrix $ cons (cons 'a' nil) (cons (cons 'b' nil) nil))
-- Matrix ["ab"]
--
transpose :: (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose m = generate $ \(i, j) -> fromJust $ m !? (j, i)
