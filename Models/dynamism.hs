{-# LANGUAGE MonadComprehensions #-}

module Anaphora where

import Model
import Nondeterminism
import Control.Monad.State

type (DynamicEntity m) = StateT [Entity] m Entity

startState :: [Entity]
startState = []

bind :: Monad m => DynamicEntity m -> DynamicEntity m
bind a = a >>= \x -> StateT $ \s -> runStateT a ([x] ++ s)

makeDynEntity :: Monad m => m Entity -> DynamicEntity m
makeDynEntity e = StateT $ \s -> [(x, s) | x <- e]

aDyn :: OnePlacePred -> DynamicEntity []
aDyn = \p -> makeDynEntity (a p)

runFromStart :: StateT [Entity] [] a -> [(a, [Entity])]
runFromStart c = runStateT c startState

fa :: StateT [Entity] [] (a -> b) -> StateT [Entity] [] a -> StateT [Entity] [] b
fa = (<*>)

ba :: StateT [Entity] [] a -> StateT [Entity] [] (a -> b) -> StateT [Entity] [] b
ba = \dx -> \df -> dx >>= \x -> df >>= \f -> return $ f x
