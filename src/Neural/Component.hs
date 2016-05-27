{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neural.Component
    ( Component(..)
    , _weights
    , componentR
    , activate
    , iterComponent
    ) where

import Control.Category
import Data.Proxy
import MyPrelude
import Prelude                     hiding (id, (.))
import Neural.Layout
import Neural.Sample
import Neural.Utils.Traversable

data Component :: (* -> *) -> (* -> *) -> * where

    Component :: Layout l => l -> Weights l Double -> Component (Source l) (Target l)

_weights :: Lens' (Component f g) [Double]
_weights = lens (\(Component _ ws) -> toList ws)
                (\(Component l _) ws -> let Just ws' = fromList ws in Component l ws')

componentR :: (Layout l, MonadRandom m) => l -> m (Component (Source l) (Target l))
componentR l = Component l <$> initR l

instance Category Component where

    id = Component idLayout None

    (Component m ws') . (Component l ws) = Component (l :> m) (Pair ws ws')

activate :: forall a. Sample a => Component (Source (Model a)) (Target (Model a)) -> a -> Output a
activate (Component l ws) = fromTarget (Proxy :: Proxy a) . compute l ws . toSource

iterComponent :: Functor t => Component f g -> Component (Iter t f) (Iter t g)
iterComponent (Component l ws) = Component (IterLayout l) ws
