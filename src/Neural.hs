{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Neural
Description : neural networks
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports all the neural network related modules for convenience.
-}

module Neural
    ( module Neural.Component
    , module Neural.Descent
    , module Neural.Layer
    , module Neural.Monad
    ) where

import Neural.Component
import Neural.Descent
import Neural.Layer
import Neural.Monad
