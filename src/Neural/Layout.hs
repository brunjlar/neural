{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Neural.Layout
    ( Layout(..)
    , NatLayout(..)
    , None(..)
    , idLayout
    , (:>)(..)
    , Pair(..)
    , Iter(..)
    , IterLayout(..)
    , FunLayout(..)
    ) where

import Control.Natural
import MyPrelude

class ( Traversable (Weights l)
      , Applicative (Weights l)) => Layout l where

    type Source l :: * -> *

    type Target l :: * -> *

    type Weights l :: * -> *

    compute :: RealFloat a => l -> Weights l a -> Source l a -> Target l a

    initR :: MonadRandom m => l -> m (Weights l Double)

data NatLayout :: (* -> *) -> (* -> *) -> * where

    NatLayout :: (f :~> g) -> NatLayout f g

data None a = None deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative None where

    pure = const None

    None <*> None = None

instance Layout (NatLayout f g) where

    type (Source (NatLayout f g)) = f

    type (Target (NatLayout f g)) = g

    type (Weights (NatLayout f g)) = None

    compute (NatLayout a) None x = a $$ x

    initR = const (return None)

idLayout :: NatLayout f f
idLayout = NatLayout (Nat id)

data Pair s t a = Pair (s a) (t a) deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (Applicative s, Applicative t) => Applicative (Pair s t) where

    pure x = Pair (pure x) (pure x)

    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

infixr 1 :>

data (:>) :: * -> * -> * where

    (:>) :: (Layout l, Layout m, Target l ~ Source m) => l -> m -> (l :> m)

instance ( Layout l
         , Layout m
         , Target l ~ Source m
         ) => Layout (l :> m) where

    type Source (l :> m) = Source l 

    type Target (l :> m) = Target m

    type Weights (l :> m) = Pair (Weights l) (Weights m)

    compute (l :> m) (Pair ws ws') = compute m ws' . compute l ws

    initR (l :> m) = Pair <$> initR l <*> initR m

newtype Iter f g a = Iter (f (g a)) deriving (Functor, Foldable, Traversable)

iterMap :: Functor f => (g a -> h a) -> Iter f g a -> Iter f h a
iterMap k (Iter xs) = Iter $ k <$> xs

newtype IterLayout (f :: * -> *) l = IterLayout l

instance (Functor f, Layout l) => Layout (IterLayout f l) where

    type Source (IterLayout f l) = Iter f (Source l)

    type Target (IterLayout f l) = Iter f (Target l)

    type Weights (IterLayout f l) = Weights l

    compute (IterLayout l) = iterMap . compute l

    initR (IterLayout l) = initR l

data FunLayout (f :: * -> *) = FunLayout (forall a. RealFloat a => a -> a)

instance Functor f => Layout (FunLayout f) where

    type Source (FunLayout f) = f

    type Target (FunLayout f) = f

    type Weights (FunLayout f) = None

    initR = const (return None)

    compute (FunLayout a) None xs = a <$> xs
