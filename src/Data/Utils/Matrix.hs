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
Module      : Data.Utils.Matrix
Description : fixed-size matrices
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixed-size /matrices/ and some basic typeclass instances and operations for them.
-}

module Data.Utils.Matrix
    ( Matrix(..) 
    , (<%%>)
    , row
    , column
    , mgenerate
    , (!!?)
    , (!!!)
    , transpose
    ) where

import GHC.TypeLits
import Data.MyPrelude
import Data.Utils.Vector

-- | @'Matrix' m n a@ is the type of /matrices/ with @m@ rows, @n@ columns and entries of type @a@.
--
newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a)) 
    deriving (Eq, Show, Functor, Foldable, Traversable, NFData)

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

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
row :: Matrix m n a -> Int -> Maybe (Vector n a)
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
column :: Matrix m n a -> Int -> Maybe (Vector m a)
column (Matrix rows) j = sequenceA $ (!? j) <$> rows

-- | Generates a matrix by applying the given function to each index (row, column).
--
-- >>> :set -XDataKinds
-- >>> mgenerate id :: Matrix 3 2 (Int, Int)
-- Matrix [[(0,0),(0,1)],[(1,0),(1,1)],[(2,0),(2,1)]]
--
mgenerate :: (KnownNat m, KnownNat n) => ((Int, Int) -> a) -> Matrix m n a
mgenerate f = Matrix $ generate (\i -> generate (\j -> f (i, j)))

-- | Gives the matrix element with the specified index (row, column) if the index is valid,
--   otherwise 'Nothing'.
--
-- >>> :set -XDataKinds
-- >>> let m = mgenerate (uncurry (+)) :: Matrix 2 3 Int
-- >>> m !!? (0,0)
-- Just 0
--
-- >>> m !!? (1, 2) 
-- Just 3
--
-- >>> m !!? (5, 7)
-- Nothing
--
(!!?) :: Matrix m n a -> (Int, Int) -> Maybe a
m !!? (i, j) = row m i >>= (!? j)

-- | Gives the matrix element with the specified index (row, column) if the index is valid,
--   otherwise throws an exception.
--
-- >>> :set -XDataKinds
-- >>> let m = mgenerate (uncurry (+)) :: Matrix 2 3 Int
-- >>> m !!! (0,0)
-- 0
--
-- >>> m !!! (1, 2) 
-- 3
--
(!!!) :: Matrix m n a -> (Int, Int) -> a
m !!! (i, j) = fromMaybe (error "Data.Utils.Matrix.!!!: invalid index") (m !!? (i, j))

-- | Transposes a matrix.
--
-- >>> transpose (Matrix $ cons (cons 'a' nil) (cons (cons 'b' nil) nil))
-- Matrix ["ab"]
--
transpose :: (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose m = mgenerate $ \(i, j) -> fromJust $ m !!? (j, i)
