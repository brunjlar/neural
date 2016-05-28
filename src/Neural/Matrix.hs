{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Neural.Matrix
    ( Matrix(..) 
    , (<%%>)
    ) where

import GHC.TypeLits
import Neural.Vector

newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a)) 
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

(<%%>) :: Num a => Matrix m n a -> Vector n a -> Vector m a
Matrix rows <%%> v = (v <%>) <$> rows
