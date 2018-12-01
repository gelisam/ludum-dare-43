{-# LANGUAGE DeriveFunctor #-}
module GameTree where

import Control.Lens hiding ((:<))
import Data.Map (Map)

import Debug


data GameTree move a = a :< Map move (GameTree move a)
  deriving Functor

gameTreeTop :: Lens' (GameTree move a) a
gameTreeTop f (a :< xs) = (:< xs) <$> f a

gameTreeSub :: Lens' (GameTree move a) (Map move (GameTree move a))
gameTreeSub f (a :< xs) = (a :<) <$> f xs


instance (Debug move, Debug a) => Debug (GameTree move a) where
  printIndented indent (a :< xs) = do
    printIndented indent a
    putIndentedStrLn indent "|_"
    printIndented (succ indent) xs
