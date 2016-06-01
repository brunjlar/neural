{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Neural.Component
    ( Component'(..)
    , weightsLens
    , activate
    , componentR
    , Component(..)
    ) where

import Control.Arrow
import Control.Category
import MyPrelude
import Neural.Analytic
import Neural.Utils.Traversable
import Prelude                  hiding (id, (.))

newtype Component' t a b = Component' { runC :: a -> t Analytic -> b }

instance Category (Component' t) where

    id = arr id

    Component' f . Component' g = Component' $ \x ts -> f (g x ts) ts

instance Arrow (Component' t) where

    arr f = Component' (\x _ -> f x)

    first (Component' f) = Component' $ \(x, y) ts -> (f x ts, y)

data Component a b = forall t. (Traversable t, Applicative t) => Component
    { weights :: t Double
    , compute :: Component' t a b
    , initR   :: forall m. MonadRandom m => m (t Double)
    }

weightsLens :: Lens' (Component a b) [Double]
weightsLens = lens (\(Component ws _ _)    -> toList ws)
                   (\(Component _  c i) ws -> let Just ws' = fromList ws in Component ws' c i)

activate :: Component a b -> a -> b
activate (Component ws (Component' f) _) x = f x $ fromDouble <$> ws

componentR :: MonadRandom m => Component a b -> m (Component a b)
componentR (Component _ c i) = i >>= \ws -> return $ Component ws c i


data Empty a = Empty deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Empty where

    pure = const Empty

    Empty <*> Empty = Empty

data Pair s t a = Pair (s a) (t a) deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (Applicative s, Applicative t) => Applicative (Pair s t) where

    pure x = Pair (pure x) (pure x)

    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

instance Category Component where

    id = arr id

    Component ws c i . Component ws' c' i' = Component
        { weights = Pair ws ws'
        , compute = Component' $ \x (Pair zs zs') -> runC c (runC c' x zs') zs 
        , initR   = Pair <$> i <*> i'
        }

instance Arrow Component where

    arr f = Component
        { weights = Empty
        , compute = arr f
        , initR   = return Empty
        }

    first (Component ws c i) = Component
        { weights = ws
        , compute = first c
        , initR   = i
        }
