{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_HADDOCK show-extensions #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

{-|
Module      : Data.FixedSize.Vector
Description : fixed-length vectors
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines fixed-length /vectors/ and some basic typeclass instances and operations for them.
-}

module Data.FixedSize.Vector
    ( Vector
    , vZipWith
    , (<%>)
    , nil
    , cons
    , vhead
    , vtail
    , (<+>)
    , (<->)
    , sqNorm
    , sqDiff
    , toVector
    , fromVector
    , KnownNat
    , natVal
    ) where

import           Data.FixedSize.Class
import           Data.MyPrelude
import           Data.Proxy
import           Data.Utils.Traversable (fromList)
import qualified Data.Vector            as V
import qualified Data.Vector.Sized      as VS
import           GHC.TypeLits

-- | @'Vector' n a@ is the type of vectors of length @n@ with elements of type @a@.
--   This is a simple wrapper around the 'VS.Vector' type from Joe Hermaszewski's
--   <https://hackage.haskell.org/package/vector-sized-0.3.3.0 vector-sized> library.
--
newtype Vector n a = Vector (VS.Vector n a)
    deriving (Eq, Functor, Applicative, Foldable, Traversable, NFData)

instance Show a => Show (Vector n a) where

    show (Vector v) = show $ VS.fromSized v

instance (KnownNat n, Read a) => Read (Vector n a) where

    readsPrec p s = let xs = readsPrec p s
                        n' = fromIntegral (natVal (Proxy :: Proxy n))
                    in  [(Vector $ fromJust $ VS.toSized ys, t) | (ys, t) <- xs, length ys == n']

instance KnownNat n => FixedSize (Vector n) where

    type Index (Vector n) = Int

    type Size (Vector n) = n

    (Vector v) !? i = VS.fromSized v V.!? i

    generate = Vector . VS.generate

-- | Function @'vZipWith'@ zips two vectors of the same length, using the specified function.
--
-- >>> :set -XDataKinds
-- >>> let f = fromJust . fromList in vZipWith div (f [6,9]) (f [2,3]) :: Vector 2 Int
-- [3,3]
--
vZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vZipWith f (Vector v) (Vector w) = Vector $ VS.zipWith f v w

-- | The /scalar product/ of two vectors of the same length.
--
-- >>> :set -XDataKinds
-- >>> cons 1 (cons 2 nil) <%> cons 3 (cons 4 nil) :: Int
-- 11
--
(<%>) :: Num a => Vector n a -> Vector n a -> a
v <%> w = sum $ vZipWith (*) v w

-- | The vector of length zero.
nil :: Vector 0 a
nil = Vector VS.empty

-- | Prepends the specified element to the specified vector.
--
-- >>> cons False (cons True nil)
-- [False,True]
--
cons :: forall a n. a -> Vector n a -> Vector (n + 1) a
cons x (Vector xs) = Vector $ VS.cons x xs

-- | Gets the first element of a vector of length greater than zero.
--
-- >>> vhead (cons 'x' (cons 'y' nil))
-- 'x'
--
vhead :: Vector (n + 1) a -> a
vhead (Vector v) = VS.head v

-- | For a vector of length greater than zero, gets the vector with its first element removed.
--
-- >>> vtail (cons 'x' (cons 'y' nil))
-- "y"
--
vtail :: forall a n. Vector (n + 1) a -> Vector n a
vtail (Vector v) = Vector $ VS.tail v

infixl 6 <+>

-- | Adds two vectors of the same length.
--
-- >>> :set -XDataKinds
-- >>> (cons 1 (cons 2 nil)) <+> (cons 3 (cons 4 nil)) :: Vector 2 Int
-- [4,6]
--
(<+>) :: Num a => Vector n a -> Vector n a -> Vector n a
(<+>) = vZipWith (+)

infixl 6 <->

-- | Subtracts two vectors of the same length.
--
-- >>> :set -XDataKinds
-- >>> (cons 1 (cons 2 nil)) <-> (cons 3 (cons 4 nil)) :: Vector 2 Int
-- [-2,-2]
--
(<->) :: Num a => Vector n a -> Vector n a -> Vector n a
(<->) = vZipWith (-)

-- | Calculates the /squared/ euclidean norm of a vector,
--   i.e. the scalar product of the vector by itself.
--
-- >>> :set -XDataKinds
-- >>> sqNorm (cons 3 (cons 4 nil)) :: Int
-- 25
--
sqNorm :: Num a => Vector n a -> a
sqNorm v = v <%> v

-- | Calculates the /squared/ euclidean distance between two vectors of the same length.
--
-- >>> :set -XDataKinds
-- >>> sqDiff (cons 1 (cons 2 nil)) (cons 3 (cons 4 nil)) :: Int
-- 8
--
sqDiff :: (Num a, KnownNat n) => Vector n a -> Vector n a -> a
sqDiff v w = sqNorm (v <-> w)

-- | Converts a fixed-size container to a 'Vector' of the same size.
--
toVector :: (FixedSize f, KnownNat (Size f)) => f a -> Vector (Size f) a
toVector = Vector . fromJust . VS.fromList . toList

-- | Converts a 'Vector' to an arbitrary fixed-size container of the same size.
--
fromVector :: FixedSize f => Vector (Size f) a -> f a
fromVector = fromJust . fromList . toList
