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
    ( module Neural.Layer
    , module Neural.Model
    , module Neural.Monad
    , module Neural.Pipes
    ) where

import Neural.Layer
import Neural.Model
import Neural.Monad
import Neural.Pipes
