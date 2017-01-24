{-# OPTIONS_HADDOCK show-extensions #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Numeric.Neural.Model
Description : "neural" components and models
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines /parameterized functions/, /components/ and /models/.
The parameterized functions are instances of the 'Arrow' and 'ArrowChoice' typeclasses, whereas
'Component's behave like 'Arrow's with choice over a different base category
(the category 'Diff' of differentiable functions).
Both parameterized functions and components can be combined easily and flexibly.

/Models/ contain a component, can measure their error with regard to samples and can be trained by gradient descent/
backpropagation.
-}

module Numeric.Neural.Model
    ( ParamFun(..)
    , Component(..)
    , _weights
    , activate
    , cArr
    , cFirst
    , cLeft
    , cConvolve
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
import Data.Functor.Compose         (Compose(..))
import Data.Functor.Product         (Product(..))
import Data.Functor.Sum             (Sum(..))
import Data.Profunctor
import Data.MyPrelude
import Prelude                      hiding (id, (.))
import Data.Utils.Analytic
import Data.Utils.Arrow
import Data.Utils.Statistics        (mean)
import Data.Utils.Traversable

-- | The type @'ParamFun' s t a b@ describes parameterized functions from @a@ to @b@, where the
--   parameters are of type @t s@.
--   When such components are composed, they all share the /same/ parameters.
--
newtype ParamFun s t a b = ParamFun { runPF :: a -> t s -> b }

instance Category (ParamFun s t) where

    id = arr id

    ParamFun f . ParamFun g = ParamFun $ \x ts -> f (g x ts) ts

instance Arrow (ParamFun s t) where

    arr f = ParamFun (\x _ -> f x)

    first (ParamFun f) = ParamFun $ \(x, y) ts -> (f x ts, y)

instance ArrowChoice (ParamFun s t) where

    left (ParamFun f) = ParamFun $ \ex ts -> case ex of
        Left x  -> Left (f x ts)
        Right y -> Right y

instance ArrowConvolve (ParamFun s t) where

    convolve (ParamFun f) = ParamFun $ \xs ts -> flip f ts <$> xs

instance Functor (ParamFun s t a) where fmap = fmapArr

instance Applicative (ParamFun s t a) where pure = pureArr; (<*>) = apArr

instance Profunctor (ParamFun s t) where dimap  = dimapArr

-- | A @'Component' f g@ is a parameterized differentiable function @f Double -> g Double@.
--   In contrast to 'ParamFun', when components are composed, parameters are not shared.
--   Each component carries its own collection of parameters instead.
--
data Component f g = forall t. (Traversable t, Applicative t, NFData (t Double)) => Component
    { weights :: t Double                                         -- ^ the specific parameter values
    , compute :: forall s. Analytic s => ParamFun s t (f s) (g s) -- ^ the encapsulated parameterized function
    , initR   :: forall m. MonadRandom m => m (t Double)          -- ^ randomly sets the parameters
    }

-- | A 'Lens'' to get or set the weights of a component.
--   The shape of the parameter collection is hidden by existential quantification,
--   so this lens has to use simple generic lists.
--
_weights:: Lens' (Component f g) [Double]
_weights = lens (\(Component ws _ _)    -> toList ws)
                (\(Component _  c i) ws -> let Just ws' = fromList ws in Component ws' c i)

-- | Activates a component, i.e. applies it to the specified input, using the current parameter values.
--
activate :: Component f g -> f Double -> g Double
activate (Component ws f _) xs = runPF f xs ws

data Empty a = Empty deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Empty where

    pure = const Empty

    Empty <*> Empty = Empty

instance NFData (Empty a) where

    rnf Empty = ()

instance (NFData (s a), NFData (t a)) => NFData (Product s t a) where

    rnf (Pair xs ys) = rnf xs `seq` rnf ys `seq` ()

instance Category Component where

    id = cArr id

    Component ws c i . Component ws' c' i' = Component
        { weights = Pair ws ws'
        , compute = ParamFun $ \x (Pair zs zs') -> runPF c (runPF c' x zs') zs
        , initR   = Pair <$> i <*> i'
        }

-- | The analogue of 'Control.Arrow.arr' for 'Component's.
--
cArr :: Diff f g -> Component f g
cArr (Diff f) = Component
    { weights = Empty
    , compute = arr f
    , initR   = return Empty
    }

-- | The analogue of 'Control.Arrow.first' for 'Component's.
--
cFirst :: Component f g -> Component (Product f h) (Product g h)
cFirst (Component ws c i) = Component
    { weights = ws
    , compute = ParamFun $ \(Pair xs ys) ws' -> Pair (runPF c xs ws') ys
    , initR   = i
    }

-- | The analogue of 'Control.Arrow.left' for 'Component's.
--
cLeft :: Component f g -> Component (Sum f h) (Sum g h)
cLeft (Component ws c i) = Component
    { weights = ws
    , compute = ParamFun $ \es ws' -> case es of
        InL xs -> InL $ runPF c xs ws'
        InR ys -> InR ys
    , initR   = i
    }

-- | The analogue of 'convolve' for 'Component's.
--
cConvolve :: Functor h => Component f g -> Component (Compose h f) (Compose h g)
cConvolve (Component ws c i) = Component
    { weights = ws
    , compute = ParamFun $ \(Compose xss) ws' -> Compose $ flip (runPF c) ws' <$> xss
    , initR   = i
    }

instance NFData (Component f g) where

    rnf (Component ws _ _) = rnf ws

-- | A @'Model' f g a b c@ wraps a @'Component' f g@
--   and models functions @b -> c@ with "samples" (for model error determination)
--   of type @a@.
--
data Model :: (* -> *) -> (* -> *) -> * -> * -> * -> * where

    Model :: (Functor f, Functor g)
             => Component f g
             -> (a -> (f Double, Diff g Identity))
             -> (b -> f Double)
             -> (g Double -> c)
             -> Model f g a b c

instance Profunctor (Model f g a) where

    dimap m n (Model c e i o) = Model c e (i . m) (n . o)

instance NFData (Model f g a b c) where

    rnf (Model c _ _ _) = rnf c

-- | A 'Lens' for accessing the component embedded in a model.
--
_component :: Lens' (Model f g a b c) (Component f g)
_component = lens (\(Model c _ _ _) -> c)
                  (\(Model _ e i o) c -> Model c e i o)

-- | Computes the modelled function.
model :: Model f g a b c -> b -> c
model (Model c _ i o) = o . activate c . i

-- | Generates a model with randomly initialized weights. All other properties are copied from the provided model.
modelR :: MonadRandom m => Model f g a b c -> m (Model f g a b c)
modelR (Model c e i o) = case c of
    Component _ f r -> do
        ws <- r
        return $ Model (Component ws f r) e i o

errFun :: forall f t a g. Functor f
          => (a -> (f Double, Diff g Identity))
          -> a
          -> (forall s. Analytic s => ParamFun s t (f s) (g s))
          -> Diff t Identity
errFun e x f = Diff $ runPF f' x where

    f' :: forall s. Analytic s => ParamFun s t a (Identity s)
    f' = proc z -> do
        let (x', Diff h) = e z
            x''          = fromDouble <$> x'
        y <- f -< x''
        returnA -< h y

modelError' :: Model f g a b c -> a -> Double
modelError' (Model c e _ _) x = case c of
    Component ws f _ -> let f' = errFun e x f
                        in  runIdentity $ runDiff f' ws

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
                                            let x          = xs' !! j
                                                (err', g') = gradWith' (\_ dw -> scale * dw) (errFun e x f) ws
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
              => Component f g
              -> (c -> Diff g Identity)
              -> (b -> f Double)
              -> (g Double -> c)
              -> StdModel f g b c
mkStdModel c e i o = Model c e' i o where

    e' (x, y) = (i x, e y)
