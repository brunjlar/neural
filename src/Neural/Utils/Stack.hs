{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Neural.Utils.Stack
    ( StackT
    , pop
    , peek
    , push
    , runStackT
    , evalStackT
    , execStackT
    , runStack
    , evalStack
    , execStack
    ) where

import Control.Monad.Trans.Class (MonadTrans)
import MyPrelude

newtype StackT s m a = StackT (StateT [s] m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

peek :: Monad m => StackT s m (Maybe s)
peek = do
    xs <- StackT get
    return $ case xs of
        []      -> Nothing
        (x : _) -> Just x

pop :: Monad m => StackT s m (Maybe s)
pop = do
    xs <- StackT get
    case xs of
        []        -> return Nothing
        (x : xs') -> (StackT $ put xs') >> return (Just x)

push :: Monad m => s -> StackT s m ()
push x = StackT $ modify (x :)

runStackT :: Monad m => StackT s m a -> [s] -> m (a, [s])
runStackT (StackT m) = runStateT m

evalStackT :: Monad m => StackT s m a -> [s] -> m a
evalStackT m xs = fst <$> runStackT m xs

execStackT :: Monad m => StackT s m a -> [s] -> m [s]
execStackT m xs = snd <$> runStackT m xs

type Stack s = StackT s Identity

runStack :: Stack s a -> [s] -> (a, [s])
runStack m xs = runIdentity $ runStackT m xs

evalStack :: Stack s a -> [s] -> a
evalStack m xs = runIdentity $ evalStackT m xs

execStack :: Stack s a -> [s] -> [s]
execStack m xs = runIdentity $ execStackT m xs
