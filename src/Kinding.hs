module Kinding where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Core



type KindSubst = Map.Map LowerName Kind

class Kinded 