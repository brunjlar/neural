{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Samples
    ( Sample(..)
    , Batch(..)
    ) where

import Data.Proxy
import Neural.Layouts
import Neural.Utils.Statistics (mean)

class ( Layout (Model a)
      , Functor (Source (Model a))) => Sample a where

    type Output a :: *

    type (Model a) :: *

    toSource :: a -> Source (Model a) Double

    fromTarget :: Proxy a -> Target (Model a) Double -> Output a

    modelError :: RealFloat b => a -> Target (Model a) b -> b

newtype Batch a = Batch [a]

instance Sample a => Sample (Batch a) where

    type Output (Batch a) = [Output a]

    type Model (Batch a) = IterLayout [] (Model a)

    toSource (Batch xs) = Iter $ toSource <$> xs

    fromTarget _ (Iter ys) = fromTarget (Proxy :: Proxy a) <$> ys

    modelError (Batch xs) (Iter ys) = mean $ zipWith modelError xs ys
