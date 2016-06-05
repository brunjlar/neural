{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Neural.Normalization
Description : normalizing data
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This modules provides utilities for data normalization.
-}

module Neural.Normalization
    ( encode1ofN
    , decode1ofN
    ) where

import MyPrelude
import Utils.Vector

-- | Provides "1 of @n@" encoding for enumerable types.
--
-- >>> :set -XDataKinds
-- >>> encode1ofN LT :: Vector 3 Int
-- [1,0,0]
--
-- >>> encode1ofN EQ :: Vector 3 Int
-- [0,1,0]
--
-- >>> encode1ofN GT :: Vector 3 Int
-- [0,0,1]
--
encode1ofN :: (Enum a, Num b, KnownNat n) => a -> Vector n b
encode1ofN x = generate $ \i -> if i == fromEnum x then 1 else 0

-- | Provides "1 of @n@" decoding for enumerable types.
--
-- >>> decode1ofN [0.9, 0.3, 0.1 :: Double] :: Ordering
-- LT
--
-- >>> decode1ofN [0.7, 0.8, 0.6 :: Double] :: Ordering
-- EQ
--
-- >>> decode1ofN [0.2, 0.3, 0.8 :: Double] :: Ordering
-- GT
--
decode1ofN :: (Enum a, Num b, Ord b, Foldable f) => f b -> a
decode1ofN = toEnum . fst . maximumBy (compare `on` snd) . zip [0..] . toList


