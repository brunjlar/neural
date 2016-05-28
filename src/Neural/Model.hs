{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Neural.Model
    ( Model(..)
    , _component
    , activateModel
    , batch
    , stdModel
    ) where

import Data.Profunctor
import MyPrelude
import Neural.Component
import Neural.Layout
import Neural.Utils.Statistics (mean)
import Neural.Vector ((<%>))

data Model f g a b = Model
    { component  :: Component f g
    , toInput    :: a -> f Double
    , fromOutput :: g Double -> b
    , modelError :: forall c. RealFloat c => (Double -> c) -> a -> g c -> c
    }

_component :: Lens' (Model f g a b) (Component f g)
_component = lens component (\m c -> m { component = c })

activateModel :: Model f g a b -> a -> b
activateModel m = fromOutput m . activateComponent (component m) . toInput m

instance Profunctor (Model f g) where

    dimap i j m = m { toInput    = toInput m . i
                    , fromOutput = j . fromOutput m
                    , modelError = \q -> modelError m q . i
                    }

batch :: Model f g a b -> Model (Iter [] f) (Iter [] g) [a] [b]
batch m = Model { component  = iterComponent (component m)
                , toInput    = \xs -> Iter $ toInput m <$> xs
                , fromOutput = \(Iter ys) -> fromOutput m <$> ys
                , modelError = \q xs (Iter ys) -> mean $ zipWith (modelError m q) xs ys
                }

stdModel :: (Applicative g, Foldable g) 
            => Component f g
            -> Model f g (f Double, g Double) (g Double) 
stdModel c = Model
    { component  = c
    , toInput    = fst
    , fromOutput = id
    , modelError = \q (_, ys) ys' -> let ds = (-) <$> (q <$> ys) <*> ys' in ds <%> ds
    }
