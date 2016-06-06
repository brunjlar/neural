{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Numeric.Neural
Description : neural networks
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module reexports all the neural network related modules for convenience.
-}

module Numeric.Neural
    ( module Numeric.Neural.Layer
    , module Numeric.Neural.Model
    , module Numeric.Neural.Normalization
    , module Numeric.Neural.Pipes
    ) where

import Numeric.Neural.Layer
import Numeric.Neural.Model
import Numeric.Neural.Normalization
import Numeric.Neural.Pipes
