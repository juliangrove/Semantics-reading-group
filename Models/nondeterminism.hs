module Nondeterminism where

import Model
import Control.Monad.List
import Control.Applicative

a :: OnePlacePred -> [Entity]
a = \p -> filter p entities
