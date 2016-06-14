{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

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

module Numeric.Neural.Model
    ( ParamFun(..)
    , Component(..)
    , _weights
    , activate
    , Model(..)
    , _component
    , model
    , modelR
    , modelError
    , descent
    , StdModel
    , mkStdModel
    ) where

import Control.Applicative    
import Control.Arrow
import Control.Category
import Control.Monad.Par            (runPar)
import Control.Monad.Par.Combinator (parMapReduceRange, InclusiveRange(..))
import Data.Profunctor
import Data.MyPrelude
import Prelude                      hiding (id, (.))
import Data.Utils.Analytic
import Data.Utils.Arrow
import Data.Utils.Statistics        (mean)
import Data.Utils.Traversable

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

-- | A @'Component' a b@ is a parameterized function from @a@ to @b@, combined with /some/ collection of analytic parameters,
--   In contrast to 'ParamFun', when components are composed, parameters are not shared. 
--   Each component carries its own collection of parameters instead.
--
data Component a b = forall t. (Traversable t, Applicative t, NFData (t Double)) => Component
    { weights :: t Double                                -- ^ the specific parameter values
    , compute :: ParamFun t a b                          -- ^ the encapsulated parameterized function
    , initR   :: forall m. MonadRandom m => m (t Double) -- ^ randomly sets the parameters
    }

-- | A 'Lens'' to get or set the weights of a component.
--   The shape of the parameter collection is hidden by existential quantification,
--   so this lens has to use simple generic lists.
--
_weights:: Lens' (Component a b) [Double]
_weights= lens (\(Component ws _ _)    -> toList ws)
               (\(Component _  c i) ws -> let Just ws' = fromList ws in Component ws' c i)

-- | Activates a component, i.e. applies it to the specified input, using the current parameter values.
--
activate :: Component a b -> a -> b
activate (Component ws f _) x = runPF f x $ fromDouble <$> ws

data Empty a = Empty deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Empty where

    pure = const Empty

    Empty <*> Empty = Empty

instance NFData (Empty a) where

    rnf Empty = ()

data Pair s t a = Pair (s a) (t a) deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData (s a), NFData (t a)) => NFData (Pair s t a) where

    rnf (Pair xs ys) = rnf xs `seq` rnf ys `seq` ()

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

instance NFData (Component a b) where

    rnf (Component ws _ _) = rnf ws

-- | A @'Model' f g a b c@ wraps a @'Component' (f 'Analytic') (g 'Analytic')@
--   and models functions @b -> c@ with "samples" (for model error determination)
--   of type @a@.
--
data Model :: (* -> *) -> (* -> *) -> * -> * -> * -> * where

    Model :: (Functor f, Functor g) 
             => Component (f Analytic) (g Analytic)
             -> (a -> (f Double, g Analytic -> Analytic)) 
             -> (b -> f Double)                          
             -> (g Double -> c)                         
             -> Model f g a b c

instance Profunctor (Model f g a) where

    dimap m n (Model c e i o) = Model c e (i . m) (n . o)

instance NFData (Model f g a b c) where

    rnf (Model c _ _ _) = rnf c

-- | A 'Lens' for accessing the component embedded in a model.
--
_component :: Lens' (Model f g a b c) (Component (f Analytic) (g Analytic))
_component = lens (\(Model c _ _ _) -> c)
                  (\(Model _ e i o) c -> Model c e i o)

-- | Computes the modelled function.
model :: Model f g a b c -> b -> c
model (Model c _ i o) = activate $ i ^>> fmap fromDouble ^>> c >>^ fmap (fromJust . fromAnalytic) >>^ o

-- | Generates a model with randomly initialized weights. All other properties are copied from the provided model. 
modelR :: MonadRandom m => Model f g a b c -> m (Model f g a b c)
modelR (Model c e i o) = case c of
    Component _ f r -> do
        ws <- r
        return $ Model (Component ws f r) e i o

errFun :: (Functor f, Traversable t)
          => (a -> (f Double, g Analytic -> Analytic))
          -> a
          -> ParamFun t (f Analytic) (g Analytic)
          -> (t Analytic -> Analytic)
errFun e x f = runPF f' x where

    f' = proc z -> do
        let (x', h) = e z
            x''     = fromDouble <$> x'
        y <- f -< x''
        returnA -< h y

modelError' :: Model f g a b c -> a -> Double
modelError' (Model c e _ _) x = case c of
    Component ws f _ -> let f'  = errFun e x f
                            f'' = fromJust . fromAnalytic . f' . fmap fromDouble
                        in  f'' ws

-- | Calculates the avarage model error for a "mini-batch" of samples.
--
modelError :: Foldable h => Model f g a b c -> h a -> Double
modelError m xs = mean $ modelError' m <$> toList xs

-- | Performs one step of gradient descent/ backpropagation on the model,
descent :: (Foldable h)
           => Model f g a b c           -- ^ the model whose error should be decreased 
           -> Double                    -- ^ the learning rate
           -> h a                       -- ^ a mini-batch of samples
           -> (Double, Model f g a b c) -- ^ returns the average sample error and the improved model
descent (Model c e i o) eta xs = case c of
    Component ws f r ->
        let xs'                       = toList xs
            l                         = length xs'
            l'                        = fromIntegral l
            scale                     = eta / l'
            q j                       = do
                                            let x          = xs' !! j --xs'' A.! j 
                                                (err', g') = gradient (\_ dw -> scale * dw) (errFun e x f) ws
                                            return (err' / l', g')
            s (err', g') (err'', g'') = return (err' + err'', (+) <$> g' <*> g'')
            (err, ws')                = runPar $ parMapReduceRange (InclusiveRange 0 $ pred l) q s (0, pure 0)
            ws''                      = (-) <$> ws <*> ws'
            c'                        = Component ws'' f r
            m                         = Model c' e i o
        in  (err, m)

-- | A type abbreviation for the most common type of models, where samples are just input-output tuples.
type StdModel f g b c = Model f g (b, c) b c

-- | Creates a 'StdModel', using the simplifying assumtion that the error can be computed from the expected
--   output allone.
--
mkStdModel :: (Functor f, Functor g) 
              => Component (f Analytic) (g Analytic)
              -> (c -> g Analytic -> Analytic)
              -> (b -> f Double)
              -> (g Double -> c)
              -> StdModel f g b c
mkStdModel c e i o = Model c e' i o where

    e' (x, y) = (i x, e y)
