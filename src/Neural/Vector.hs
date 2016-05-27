{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Neural.Vector
    ( Vector(..)
    , (<%>)
    ) where

import GHC.TypeLits
import MyPrelude

infixr 5 :%

data Vector :: Nat -> * -> * where
    Nil  :: Vector 0 a
    (:%) :: a -> Vector (n - 1) a -> Vector n a

deriving instance Eq a => Eq (Vector n a)

deriving instance Functor (Vector n)

deriving instance Foldable (Vector n)

deriving instance Traversable (Vector n)

instance Show a => Show (Vector n a) where

    show v = "<" ++ intercalate ", " (map show $ toList v) ++ ">"

instance Applicative (Vector 0) where

    pure = const Nil

    _ <*> _ = Nil

instance Applicative (Vector (n - 1)) => Applicative (Vector n) where

    pure x = x :% pure x

    (f :% fs) <*> (x :% xs) = f x :% (fs <*> xs)
    _         <*> _         = error "impossible branch"

(<%>) :: (Applicative (Vector n), Num a) => Vector n a -> Vector n a -> a
v <%> w = sum $ (*) <$> v <*> w
