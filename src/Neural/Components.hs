{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Neural.Components
    ( Component(..)
    , _weights
    , activate
    , componentR
    ) where

import Control.Category
import MyPrelude
import Prelude                     hiding (id, (.))
import Neural.Layouts
import Neural.Utils.Traversable

data Component :: (* -> *) -> (* -> *) -> * where

    Component :: Layout l => l -> Weights l Double -> Component (Source l) (Target l)

_weights :: Lens' (Component f g) [Double]
_weights = lens (\(Component _ ws) -> toList ws)
                (\(Component l _) ws -> let Just ws' = fromList ws in Component l ws')

activate :: Component f g -> f Double -> g Double
activate (Component l ws) xs = compute l ws xs

componentR :: (Layout l, MonadRandom m) => l -> m (Component (Source l) (Target l))
componentR l = Component l <$> initR l

instance Category Component where

    id = Component idLayout None

    (Component m ws') . (Component l ws) = Component (l :> m) (Pair ws ws')
