{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Monad
    ( ComponentM
    , runComponentM
    , evalComponentM
    , activateM
    , randomizeM
    ) where

import MyPrelude
import Neural.Component

newtype ComponentM a b m c = ComponentM (StateT (Component a b) (RandT StdGen m) c)
    deriving (Functor, Applicative, Monad, MonadState (Component a b), MonadRandom, MonadIO)

runComponentM :: Monad m 
                 => Component a b
                 -> StdGen 
                 -> ComponentM a b m c
                 -> m (c, Component a b, StdGen)
runComponentM c g (ComponentM m) = 
    runRandT (runStateT m c) g >>= \((x, c'), g') -> 
    return (x, c', g')

evalComponentM :: Monad m 
                  => Component a b
                  -> StdGen 
                  -> ComponentM a b m c
                  -> m c
evalComponentM c g m = runComponentM c g m >>= \(x, _, _) -> return x

activateM :: Monad m => a -> ComponentM a b m b
activateM x = get >>= \c -> return $ activate c x

randomizeM :: Monad m => ComponentM a b m ()
randomizeM = do
    Component _ c i <- get
    ws              <- i
    put (Component ws c i)
