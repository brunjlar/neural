{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Utils.Arrow
Description : arrow utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines utility functions for /arrows/.
-}

module Utils.Arrow
    ( ArrowConvolve(..)
    , fmapArr
    , pureArr
    , apArr
    , dimapArr
    ) where

import Control.Arrow

-- | Arrows implementing 'ArrowConvolve' can be mapped over containers.
--   This means that every functor (@f :: Hask -> Hask@) lifts to a functor (@a -> a@).
--
--   Instances should satisfy the following laws:
--
--   * @convolve id = id@
--
--   * @convolve (g . h) = convolve g . convolve h@
--
--   * @convolve . arr = arr . fmap@
--
class Arrow a => ArrowConvolve a where

    convolve :: forall f b c. Functor f => a b c -> a (f b) (f c)

-- | A function suitable to define the canonical 'Functor' instance for arrows.
--
fmapArr :: Arrow a => (c -> d) -> a b c -> a b d
fmapArr f a = a >>^ f

-- | A function to define 'pure' for arrows. 
-- Combining this with 'apArr', the canonical 'Applicative' instance for arrows can easily be defined.
--
pureArr :: Arrow a => c -> a b c
pureArr = arr . const

-- | A function to define @('<*>')@ for arrows.
-- Combining this with 'pureArr', the canonical 'Applicative' instance for arrows can easily be defined.
--
apArr :: Arrow a => a b (c -> d) -> a b c -> a b d
apArr a b = proc x -> do
    f <- a -< x
    y <- b -< x
    returnA -< f y

-- | A function suitable to define the canonical 'Data.Profunctor.Profunctor' instance for arrows.
--
dimapArr :: Arrow a => (b -> c) -> (d -> e) -> a c d -> a b e
dimapArr f g a = f ^>> a >>^ g
