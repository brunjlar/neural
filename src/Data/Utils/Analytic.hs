{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Data.Utils.Analytic
Description : "analytic" values
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the numeric class 'Analytic', "differentiable" functions @'Diff' f g@ 
and an adapted version of 'Numeric.AD.gradWith''.
-}

module Data.Utils.Analytic
    ( Analytic(..)
    , Diff(..)
    , Diff'
    , diff
    , gradWith'
    ) where

import           Control.Category
import           Data.MyPrelude
import           Data.Reflection             (Reifies)
import qualified Numeric.AD                  as AD                  
import           Numeric.AD.Internal.Reverse (Reverse, Tape)
import Prelude                               hiding (id, (.))

-- | Class 'Analytic' is a helper class for defining differentiable functions.
--
class (Floating a, Ord a) => Analytic a where

    fromDouble :: Double -> a

instance Analytic Double where

    fromDouble = id

instance Reifies s Tape => Analytic (Reverse s Double) where

    fromDouble = AD.auto

-- | Type @'Diff' f g@ can be thought of as the type of "differentiable" functions @f Double -> g Double@.
newtype Diff f g = Diff { runDiff :: forall a. Analytic a => f a -> g a }

instance Category Diff where

    id = Diff id

    Diff f . Diff g = Diff (f . g)

-- | Type @'Diff''@ can be thought of as the type of differentiable functions
--   @Double -> Double@.
type Diff' = forall a. Analytic a => a -> a

-- | Lifts a differentiable function by pointwise application.
--
diff :: Functor f => Diff' -> Diff f f
diff f = Diff (fmap f)

-- | Computes the gradient of an analytic function and combines it with the argument. 
--
-- >>> gradWith' (\_ d -> d) (Diff $ \[x, y] -> Identity $ x * x + 3 * y + 7) [2, 1]
-- (14.0,[4.0,3.0])
--
gradWith' :: Traversable t 
             => (Double -> Double -> a) -- ^ how to combine argument and gradient
             -> Diff t Identity         -- ^ differentiable function
             -> t Double                -- ^ function argument
             -> (Double, t a)           -- ^ function value and combination of argument and gradient
gradWith' c f = AD.gradWith' c (runIdentity . runDiff f)
