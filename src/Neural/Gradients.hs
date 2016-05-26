{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Gradients
    ( 
    ) where

import Control.Category
import Control.Natural
import MyPrelude
import Numeric.AD
import Prelude                     hiding (id, (.))
import Neural.Utils.Traversable

class ( Traversable (Weights l)
      , Applicative (Weights l)) => Layout l where

    type Source l :: * -> *

    type Target l :: * -> *

    type Weights l :: * -> *

    compute :: RealFloat a => l -> Weights l a -> Source l a -> Target l a

    initR :: MonadRandom m => l -> m (Weights l Double)

data Component :: (* -> *) -> (* -> *) -> * where

    Component :: Layout l => l -> Weights l Double -> Component (Source l) (Target l)

_weights :: Lens' (Component f g) [Double]
_weights = lens (\(Component _ ws) -> toList ws)
                (\(Component l _) ws -> let Just ws' = fromList ws in Component l ws')

activate :: Component f g -> f Double -> g Double
activate (Component l ws) xs = compute l ws xs

componentR :: (Layout l, MonadRandom m) => l -> m (Component (Source l) (Target l))
componentR l = Component l <$> initR l

descent :: Functor f => Component f Identity -> Double -> f Double -> Component f Identity
descent (Component l ws) eta xs = Component l ws' where

    ws' = gradWith
            (\x dx -> x - eta * dx)
            (\ws'' -> runIdentity $ compute l ws'' $ auto <$> xs)
            ws

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

instance Category Component where

    id = Component idLayout None

    (Component m ws') . (Component l ws) = Component (l :> m) (Pair ws ws')
