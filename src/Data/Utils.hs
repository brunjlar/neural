{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Utils
Description : various utilities
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports various utility modules for convenience.
-}

module Data.Utils
    ( module Data.FixedSize 
    , module Data.Utils.Analytic
    , module Data.Utils.Arrow
    , module Data.Utils.Cache
    , module Data.Utils.Pipes
    , module Data.Utils.Random
    , module Data.Utils.Stack
    , module Data.Utils.Statistics
    , module Data.Utils.Traversable
    ) where

import Data.FixedSize
import Data.Utils.Analytic
import Data.Utils.Arrow
import Data.Utils.Cache
import Data.Utils.Pipes
import Data.Utils.Random
import Data.Utils.Stack
import Data.Utils.Statistics
import Data.Utils.Traversable
