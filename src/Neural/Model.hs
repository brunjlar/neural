{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-|
Module      : Neural.Model
Description : "neural" components and models
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines /parameterized functions/, /components/ and /models/. The parameterized functions and components
are instances of the 'Arrow' typeclass and can therefore be combined easily and flexibly. 

/Models/ contain a component, can measure their error with regard to samples and can be trained by gradient descent/
backpropagation.
-}

module Neural.Model
    ( ParamFun(..)
    , Component(..)
    , weightsLens
    , activate
    , CTransform
    , Err
    , withCTransform
    , Model(..)
    , model
    , modelError
    , modelR
    ) where

import Control.Arrow
import Control.Category
import Data.Profunctor
import MyPrelude
import Prelude           hiding (id, (.))
import Utils.Analytic
import Utils.Arrow
import Utils.Statistics  (mean)
import Utils.Traversable

-- | The type @'ParamFun' t a b@ describes parameterized functions from @a@ to @b@, where the
--   parameters are of type @t 'Analytic'@.
--   When such components are composed, they all share the /same/ parameters.
--
newtype ParamFun t a b = ParamFun { runPF :: a -> t Analytic -> b }

instance Category (ParamFun t) where

    id = arr id

    ParamFun f . ParamFun g = ParamFun $ \x ts -> f (g x ts) ts

instance Arrow (ParamFun t) where

    arr f = ParamFun (\x _ -> f x)

    first (ParamFun f) = ParamFun $ \(x, y) ts -> (f x ts, y)

instance ArrowChoice (ParamFun t) where

    left (ParamFun f) = ParamFun $ \ex ts -> case ex of
        Left x  -> Left (f x ts)
        Right y -> Right y

instance ArrowConvolve (ParamFun t) where

    convolve (ParamFun f) = ParamFun $ \xs ts -> flip f ts <$> xs

instance Functor (ParamFun t a) where fmap = fmapArr

instance Applicative (ParamFun t a) where pure = pureArr; (<*>) = apArr

instance Profunctor (ParamFun t) where dimap  = dimapArr

-- | A @'Model' a b@ is a parameterized function from @a@ to @b@, combined with /some/ collection of analytic parameters,
--   In contrast to 'ParamFun', when components are composed, parameters are not shared. 
--   Each component carries its own collection of parameters instead.
--
data Component a b = forall t. (Traversable t, Applicative t) => Component
    { weights :: t Double                                -- ^ the specific parameter values
    , compute :: ParamFun t a b                          -- ^ the encapsulated parameterized function
    , initR   :: forall m. MonadRandom m => m (t Double) -- ^ randomly sets the parameters
    }

-- | A 'Lens'' to get or set the weights of a component.
--   The shape of the parameter collection is hidden by existential quantification,
--   so this lens has to use simple generic lists.
--
weightsLens :: Lens' (Component a b) [Double]
weightsLens = lens (\(Component ws _ _)    -> toList ws)
                   (\(Component _  c i) ws -> let Just ws' = fromList ws in Component ws' c i)

-- | Activates a component, i.e. applies it to the specified input, using the current parameter values.
--
activate :: Component a b -> a -> b
activate (Component ws f _) x = runPF f x $ fromDouble <$> ws

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
        , compute = ParamFun $ \x (Pair zs zs') -> runPF c (runPF c' x zs') zs 
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

instance ArrowChoice Component where

    left (Component ws c i) = Component ws (left c) i

instance ArrowConvolve Component where

    convolve (Component ws c i) = Component ws (convolve c) i

instance Functor (Component a) where fmap = fmapArr

instance Applicative (Component a) where pure = pureArr; (<*>) = apArr

instance Profunctor Component where dimap = dimapArr

-- | A 'CTransform' can be thought of as a transformation of components that preserves the parameter collection.
--
type CTransform a b c d = forall t. ParamFun t a b -> ParamFun t c d

-- | Function 'withCTransform' temporarily "unwraps" a component to get to the encapsulated parameterized function,
--   applies the 'CTransform' to get a transformed component with the same weight collection,
--   then applies an arbitrary function to the transformed component.
--
withCTransform :: CTransform a b c d      -- ^ the transformation
                  -> Component a b        -- ^ the component to transform
                  -> (Component c d -> e) -- ^ the function to apply to the transformed component
                  -> e                    
withCTransform t (Component ws c i) f = f $ Component ws (t c) i

-- | An element @err@ of @'Err' a b c@ can be used to measure the error of a component.
--   This makes it possible to later use gradient descent to minimize @c@'s error with respect to @err@.
--
type Err a b c = CTransform a b c Analytic

-- | A @'Model' a b c d e@ models functions @d -> e@. 
--   It encapsulates a component from @a@ to @b@ and can measure errors on samples of type @c@.
--
data Model a b c d e = Model
    { component :: Component a b      -- ^ the underlying component
    , err       :: Err a b c          -- ^ measures the model error
    , finalize  :: CTransform a b d e -- ^ adapts the component to the right type for modelling purposes
    } 

-- | Evaluates a model.
model :: Model a b c d e -> d -> e
model m x = withCTransform (finalize m) (component m) (`activate` x)

-- | Gives the average model error for the specified samples.
--
modelError :: Foldable f => Model a b c d e -> f c -> Analytic
modelError m xs = withCTransform (err m) (component m) $ \c -> mean $ activate c <$> toList xs

-- | Sets the parameters randomly, but keeps all other properties of a component.
--
modelR :: MonadRandom m => Model a b c d e -> m (Model a b c d e)
modelR m = case component m of
    Component _ c i -> do
        ws <- i
        return $ m { component = Component ws c i }
