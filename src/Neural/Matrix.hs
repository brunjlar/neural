{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Neural.Matrix
    ( Matrix(..) 
    , (<%%>)
    , mindex
    ) where

import Data.Proxy
import GHC.TypeLits
import Neural.Vector

newtype Matrix (m :: Nat) (n :: Nat) a = Matrix (Vector m (Vector n a)) 
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance (KnownNat m, KnownNat n) => Applicative (Matrix m n) where

    pure x = Matrix $ pure (pure x)

    Matrix fs <*> Matrix xs = Matrix $ (<*>) <$> fs <*> xs

(<%%>) :: Num a => Matrix m n a -> Vector n a -> Vector m a
Matrix rows <%%> v = (v <%>) <$> rows

mindex :: (KnownNat i, KnownNat j, i <= m - 1, j <= n - 1) => Matrix m n a -> Proxy i -> Proxy j -> a
mindex (Matrix rows) p = vindex (vindex rows p)
