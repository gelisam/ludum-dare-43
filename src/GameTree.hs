module GameTree where

import Control.Applicative
import Control.Comonad.Cofree
import Control.Lens hiding ((:<))
import Data.Foldable
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map


type GameTree move = Cofree (Map move)

gameTreeTop :: Lens' (GameTree move a) a
gameTreeTop f (a :< xs) = (:< xs) <$> f a

gameTreeSub :: Lens' (GameTree move a) (Map move (GameTree move a))
gameTreeSub f (a :< xs) = (a :<) <$> f xs
