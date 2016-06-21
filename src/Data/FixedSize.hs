{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.FixedSize
Description : fixed-size containers
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module provides some fixed-size containers.
-}

module Data.FixedSize
    ( module Data.FixedSize.Class
    , module Data.FixedSize.Matrix
    , module Data.FixedSize.Vector
    , module Data.FixedSize.Volume
    ) where

import Data.FixedSize.Class
import Data.FixedSize.Matrix
import Data.FixedSize.Vector
import Data.FixedSize.Volume
