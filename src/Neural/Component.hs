{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-|
Module      : Neural.Component
Description : "neural" components
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines /components/, which are parameterized functions which can be trained by backpropagation.
By using the 'Arrow' instance, small components can easily by combined to bigger, more complicated components.
-}

module Neural.Component
    ( Component'(..)
    , Component(..)
    , weightsLens
    , activate
    , componentR
    ) where

import Control.Arrow
import Control.Category
import MyPrelude
import Prelude           hiding (id, (.))
import Utils.Analytic
import Utils.Traversable

-- | The type @'Component'' t a b@ describes parameterized functions from @a@ to @b@, where the
--   parameters are of type @t 'Analytic'@.
--   When such components are composed, they all share the /same/ parameters.
--
newtype Component' t a b = Component' { runC :: a -> t Analytic -> b }

instance Category (Component' t) where

    id = arr id

    Component' f . Component' g = Component' $ \x ts -> f (g x ts) ts

instance Arrow (Component' t) where

    arr f = Component' (\x _ -> f x)

    first (Component' f) = Component' $ \(x, y) ts -> (f x ts, y)

-- | A @'Component' a b@ is a parameterized function from @a@ to @b@ for /some/ collection of analytic parameters.
--   In contrast to 'Component'', when these components are composed, each component carries its own
--   collection of parameters.
--
data Component a b = forall t. (Traversable t, Applicative t) => Component
    { weights :: t Double                                -- ^ The specific parameter values.
    , compute :: Component' t a b                        -- ^ The encapsulated parameterized function.
    , initR   :: forall m. MonadRandom m => m (t Double) -- ^ Randomly sets the parameters.
    }

-- | A 'Lens'' to get or set the weights of a component.
--   The shape of the parameter collection is hidden by existential quantification,
--   so instead this lens uses simple lists.
--
weightsLens :: Lens' (Component a b) [Double]
weightsLens = lens (\(Component ws _ _)    -> toList ws)
                   (\(Component _  c i) ws -> let Just ws' = fromList ws in Component ws' c i)

-- | Activates a component, i.e. applies it to the specified input, using the current parameter values.
--
activate :: Component a b -> a -> b
activate (Component ws (Component' f) _) x = f x $ fromDouble <$> ws

-- | Sets the parameters randomly, but keeps all other properties of a component.
--
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
