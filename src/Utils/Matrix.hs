{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Matrix
    ( Matrix(..) 
    , (<%%>)
    , row
    , column
    , mgenerate
    , (!!?)
    , transpose
    ) where

import GHC.TypeLits
import MyPrelude
import Utils.Vector

newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a)) 
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

(<%%>) :: Num a => Matrix m n a -> Vector n a -> Vector m a
Matrix rows <%%> v = (v <%>) <$> rows

row :: Matrix m n a -> Int -> Maybe (Vector n a)
row (Matrix rows) = (rows !?)

column :: Matrix m n a -> Int -> Maybe (Vector m a)
column (Matrix rows) j = sequenceA $ (!? j) <$> rows

mgenerate :: (KnownNat m, KnownNat n) => ((Int, Int) -> a) -> Matrix m n a
mgenerate f = Matrix $ generate (\i -> generate (\j -> f (i, j)))

(!!?) :: Matrix m n a -> (Int, Int) -> Maybe a
m !!? (i, j) = row m i >>= (!? j)

transpose :: (KnownNat m, KnownNat n) => Matrix m n a -> Matrix n m a
transpose m = mgenerate $ \(i, j) -> fromJust $ m !!? (j, i)
