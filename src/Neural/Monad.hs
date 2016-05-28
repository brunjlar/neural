{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Monad
    ( ModelM
    , runModelM
    , evalModelM
    , runInitModelM
    , evalInitModelM
    , activateM
    ) where

import MyPrelude
import Neural.Component
import Neural.Layout
import Neural.Model

newtype ModelM f g a b m c = ModelM (StateT (Model f g a b) (RandT StdGen m) c)
    deriving (Functor, Applicative, Monad, MonadState (Model f g a b), MonadRandom, MonadIO)

runModelM :: Monad m 
             => Model f g a b
             -> StdGen 
             -> ModelM f g a b m c 
             -> m (c, Model f g a b, StdGen)
runModelM m g (ModelM mx) = 
    runRandT (runStateT mx m) g >>= \((x, m'), g') -> 
    return (x, m', g')

evalModelM :: Monad m 
              => Model f g a b
              -> StdGen 
              -> ModelM f g a b m c 
              -> m c
evalModelM m g mx = runModelM m g mx >>= \(x, _, _) -> return x

runInitModelM :: Monad m
                 => LAYOUT f g
                 -> (Component f g -> Model f g a b)
                 -> StdGen
                 -> ModelM f g a b m d
                 -> m (d, Model f g a b, StdGen)
runInitModelM l mkModel g mx =
    runModelM undefined g mx' where

        mx' = do
            c <- componentR l
            put $ mkModel c
            mx

evalInitModelM :: Monad m
                  => LAYOUT f g
                  -> (Component f g -> Model f g a b)
                  -> StdGen
                  -> ModelM f g a b m d
                  -> m d
evalInitModelM l mkModel g mx =
    runInitModelM l mkModel g mx >>= \(x, _, _) -> return x

activateM :: Monad m => a -> ModelM f g a b m b
activateM xs = get >>= \m -> return $ activateModel m xs
