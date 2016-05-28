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
    , LAYOUT(..)
    , Empty(..)
    , analytic
    , Pair(..)
    , PairLayout(..)
    , Iter(..)
    , IterLayout(..)
    ) where

import Control.Category
import MyPrelude
import Neural.Analytic
import Prelude          hiding (id, (.))

class (Traversable (Weights l), Applicative (Weights l)) => Layout l where

    type Source l :: * -> *

    type Target l :: * -> *

    type Weights l :: * -> *

    compute :: RealFloat a => l -> Weights l a -> Source l a -> Target l a

    initR :: MonadRandom m => l -> m (Weights l Double)

data LAYOUT :: (* -> *) -> (* -> *) -> * where

    LAYOUT :: Layout l => l -> LAYOUT (Source l) (Target l)

data Empty a = Empty deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Empty where

    pure = const Empty

    Empty <*> Empty = Empty

instance Layout (Analytic f g) where

    type (Source (Analytic f g)) = f

    type (Target (Analytic f g)) = g

    type (Weights (Analytic f g)) = Empty

    compute (Analytic a) Empty = a

    initR = const (return Empty)

analytic :: Analytic f g -> LAYOUT f g
analytic = LAYOUT

data Pair s t a = Pair (s a) (t a) deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (Applicative s, Applicative t) => Applicative (Pair s t) where

    pure x = Pair (pure x) (pure x)

    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

data PairLayout :: * -> * -> * where

    PairLayout :: (Layout l, Layout m, Source l ~ Target m) => l -> m -> PairLayout l m

instance ( Layout l
         , Layout m
         , Source l ~ Target m
         ) => Layout (PairLayout l m) where

    type Source (PairLayout l m) = Source m 

    type Target (PairLayout l m) = Target l

    type Weights (PairLayout l m) = Pair (Weights l) (Weights m)

    compute (PairLayout l m) (Pair ws ws') = compute l ws . compute m ws'

    initR (PairLayout l m) = Pair <$> initR l <*> initR m

instance Category LAYOUT where

    id = analytic id

    LAYOUT l . LAYOUT m = LAYOUT (PairLayout l m)

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
