{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Neural.Component
    ( Component(..)
    , _weights
    , componentR
    , activateComponent
    , iterComponent
    ) where

import Control.Category
import MyPrelude
import Prelude                  hiding (id, (.))
import Neural.Analytic
import Neural.Layout
import Neural.Utils.Traversable

data Component :: (* -> *) -> (* -> *) -> * where

    Component :: Layout l => l -> Weights l Double -> Component (Source l) (Target l)

_weights :: Lens' (Component f g) [Double]
_weights = lens (\(Component _ ws) -> toList ws)
                (\(Component l _) ws -> let Just ws' = fromList ws in Component l ws')

componentR :: MonadRandom m => LAYOUT f g -> m (Component f g)
componentR (LAYOUT l) = Component l <$> initR l

instance Category Component where

    id = Component (Analytic id) Empty

    (Component m ws) . (Component l ws') = Component (PairLayout m l) (Pair ws ws')

activateComponent :: Component f g -> f Double -> g Double
activateComponent (Component l ws) = compute l ws

iterComponent :: Functor t => Component f g -> Component (Iter t f) (Iter t g)
iterComponent (Component l ws) = Component (IterLayout l) ws
