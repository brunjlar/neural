{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Neural.Vector
    ( Vector
    , (<%>)
    , nil
    , cons
    ) where

import           Data.Proxy
import qualified Data.Vector            as V
import           GHC.TypeLits
import           GHC.TypeLits.Witnesses
import           MyPrelude

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

    Vector fs <*> Vector xs = Vector (fs <*> xs)

instance Foldable (Vector n) where

    foldMap f (Vector xs) = foldMap f xs

instance Traversable (Vector n) where

    sequenceA (Vector xs) = Vector <$> sequenceA xs

instance (KnownNat n, Read a) => Read (Vector n a) where

    readsPrec p s = let xs  = readsPrec p s :: [(V.Vector a, String)]
                        n'  = fromIntegral (natVal (Proxy :: Proxy n))
                    in  [(Vector ys, t) | (ys, t) <- xs, length ys == n']    

(<%>) :: (Num a, Foldable f) => f a -> f a -> a
xs <%> ys = sum $ zipWith (*) (toList xs) (toList ys)

nil :: Vector 0 a
nil = Vector V.empty

cons :: forall a n. a -> Vector n a -> Vector (n + 1) a
cons x (Vector xs) = withNatOp (%+) (Proxy :: Proxy n) (Proxy :: Proxy 1) $ Vector $ V.cons x xs
