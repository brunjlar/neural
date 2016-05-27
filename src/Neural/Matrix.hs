{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Matrix
    ( Matrix(..) 
    , (<%%>)
    ) where

import GHC.TypeLits
import Neural.Vector

newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a)) 
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Applicative (Vector m), Applicative (Vector n)) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

(<%%>) :: (Applicative (Vector n), Num a) => Matrix m n a -> Vector n a -> Vector m a
Matrix rows <%%> v = (v <%>) <$> rows

