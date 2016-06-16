{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Utils.Stack
Description : a simple stack monad
Copyright   : (c) Lars Brünjes, 2016
License     : MIT
Maintainer  : brunjlar@gmail.com
Stability   : experimental
Portability : portable

This module defines the 'StackT' monad transformer,
which is simply a wrapped state monad whose state is a list.
-}

module Data.Utils.Stack
    ( StackT
    , pop
    , peek
    , push
    , runStackT
    , evalStackT
    , execStackT
    , Stack
    , runStack
    , evalStack
    , execStack
    ) where

import Control.Monad.Trans.Class (MonadTrans)
import Data.MyPrelude

-- | A computation of type @'StackT' s m a@ has access to a stack of elements of type @s@.
--
newtype StackT s m a = StackT (StateT [s] m a)
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Peeks at the top element of the stack. Returns 'Nothing' if the stack is empty.
--
peek :: Monad m => StackT s m (Maybe s)
peek = do
    xs <- StackT get
    return $ case xs of
        []      -> Nothing
        (x : _) -> Just x

-- | Pops the top element from the stack. Returns 'Nothing' if the stack is empty.
--
pop :: Monad m => StackT s m (Maybe s)
pop = do
    xs <- StackT get
    case xs of
        []        -> return Nothing
        (x : xs') -> (StackT $ put xs') >> return (Just x)

-- | Pushes a new element onto the stack.
--
push :: Monad m => s -> StackT s m ()
push x = StackT $ modify (x :)

-- | Runs a computation in the @'StackT' s m@ monad.
--
runStackT :: StackT s m a -> [s] -> m (a, [s])
runStackT (StackT m) = runStateT m

-- | Evaluates a computation in the @'StackT' s m@ monad.
--
evalStackT :: Monad m => StackT s m a -> [s] -> m a
evalStackT m xs = fst <$> runStackT m xs

-- | Executes a computation in the @'StackT' s m@ monad.
--
execStackT :: Monad m => StackT s m a -> [s] -> m [s]
execStackT m xs = snd <$> runStackT m xs

-- | A pure stack monad.
--
type Stack s = StackT s Identity

-- | Runs a computation in the @'Stack' s@ monad.
--
runStack :: Stack s a -> [s] -> (a, [s])
runStack m xs = runIdentity $ runStackT m xs

-- | Evaluates a computation in the @'Stack' s@ monad.
--
evalStack :: Stack s a -> [s] -> a
evalStack m xs = runIdentity $ evalStackT m xs

-- | Executes a computation in the @'Stack' s@ monad.
--
execStack :: Stack s a -> [s] -> [s]
execStack m xs = runIdentity $ execStackT m xs
