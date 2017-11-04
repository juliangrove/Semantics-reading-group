module Anaphora where

import Model
import Nondeterminism
import Control.Monad.State

type DynamicEntity = StateT [Entity] [] Entity

startState :: [Entity]
startState = []

bind :: DynamicEntity -> DynamicEntity
bind a = a >>= \x -> StateT $ \s -> runStateT a ([x] ++ s)

makeDynEntity :: [Entity] -> DynamicEntity
makeDynEntity e = StateT $ \s -> [(x, s) | x <- e]

aDyn :: OnePlacePred -> DynamicEntity
aDyn = \p -> makeDynEntity (a p)

runFromStart :: StateT [Entity] [] a -> [(a, [Entity])]
runFromStart c = runStateT c startState
