{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Utils.Vector
Description : fixed-length vectors
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixed-length /vectors/ and some basic typeclass instances and operations for them.
-}

module Utils.Vector
    ( Vector
    , (<%>)
    , nil
    , cons
    , generate
    , (!?)
    , vhead
    , vtail
    ) where

import           Data.Proxy
import qualified Data.Vector            as V
import           GHC.TypeLits
import           GHC.TypeLits.Witnesses
import           MyPrelude

-- | @'Vector' n a@ is the type of vectors of length @n@ with elements of type @a@.
data Vector :: Nat -> * -> * where

    Vector :: KnownNat n => V.Vector a -> Vector n a

instance Eq a => Eq (Vector n a) where

    Vector xs == Vector ys = xs == ys

instance Show a => Show (Vector n a) where

    showsPrec p (Vector xs) = showsPrec p xs

instance Functor (Vector n) where

    fmap f (Vector v) = Vector (f <$> v)

instance forall n. KnownNat n => Applicative (Vector n) where

    pure x = let n = natVal (Proxy :: Proxy n) in Vector (V.replicate (fromIntegral n) x)

    Vector fs <*> Vector xs = Vector (V.zipWith ($) fs xs)

instance Foldable (Vector n) where

    foldMap f (Vector xs) = foldMap f xs

instance Traversable (Vector n) where

    sequenceA (Vector xs) = Vector <$> sequenceA xs

instance (KnownNat n, Read a) => Read (Vector n a) where

    readsPrec p s = let xs  = readsPrec p s :: [(V.Vector a, String)]
                        n'  = fromIntegral (natVal (Proxy :: Proxy n))
                    in  [(Vector ys, t) | (ys, t) <- xs, length ys == n']    

-- | The /scalar product/ of two vectors of the same length.
--
-- >>> :set -XDataKinds
-- >>> cons 1 (cons 2 nil) <%> cons 3 (cons 4 nil) :: Int
-- 11
--
(<%>) :: Num a => Vector n a -> Vector n a -> a
xs <%> ys = sum $ zipWith (*) (toList xs) (toList ys)

-- | The vector of length zero.
nil :: Vector 0 a
nil = Vector V.empty

-- | Prepends the specified element to the specified vector.
--
-- >>> cons False (cons True nil)
-- [False,True]
--
cons :: forall a n. a -> Vector n a -> Vector (n + 1) a
cons x (Vector xs) = withNatOp (%+) (Proxy :: Proxy n) (Proxy :: Proxy 1) $ Vector $ V.cons x xs

-- | Generates a vector by applying the given function to each index.
--
-- >>> :set -XDataKinds
-- >>> generate id :: Vector 3 Int
-- [0,1,2]
--
generate :: forall n a. KnownNat n => (Int -> a) -> Vector n a
generate = Vector . V.generate (fromIntegral $ natVal (Proxy :: Proxy n))

-- | Gets the vector element at the specified index if the index is valid, otherwise 'Nothing'.
--
-- >>> cons 'x' nil !? 0
-- Just 'x'
--
-- >>> cons 'x' nil !? 1
-- Nothing
--
(!?) :: Vector n a -> Int -> Maybe a
Vector v !? i = v V.!? i

-- | Gets the first element of a vector of length greater than zero.
--
-- >>> vhead (cons 'x' (cons 'y' nil))
-- 'x'
--
vhead :: (1 <= n) => Vector n a -> a
vhead (Vector v) = V.head v

-- | For a vector of length greater than zero, gets the vector with its first element removed.
--
-- >>> vtail (cons 'x' (cons 'y' nil))
-- "y"
--
vtail :: forall a n. (1 <= n) => Vector n a -> Vector (n - 1) a
vtail (Vector v) = withNatOp (%-) (Proxy :: Proxy n) (Proxy :: Proxy 1) $ Vector (V.tail v)
