module Utils.Traversable
    ( fromList
    , safeHead
    ) where

import MyPrelude
import Utils.Stack

fromList :: (Applicative t, Traversable t) => [a] -> Maybe (t a)
fromList xs = sequenceA $ evalStack (sequenceA $ pure pop) xs

safeHead :: [a] -> Maybe a
safeHead = (runIdentity <$>) . fromList
