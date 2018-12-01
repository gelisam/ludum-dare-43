{-# LANGUAGE DeriveFunctor #-}
module GameTree where

import Prelude hiding (take)
import Control.Lens hiding ((:<))
import Data.Map (Map)
import qualified Data.Map as Map

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


take :: Ord move
     => Int -> GameTree move a -> GameTree move a
take 0 (a :< _ ) = a :< mempty
take n (a :< xs) = a :< (take (n - 1) <$> xs)

takeUntil :: Ord move
          => (a -> Bool) -> GameTree move a -> GameTree move a
takeUntil p (a :< xs) | p a       = a :< mempty
                      | otherwise = a :< fmap (takeUntil p) xs

mapMoves :: (Ord move, Ord move')
         => (move -> move') -> GameTree move a -> GameTree move' a
mapMoves f (a :< xs) = a :< xs'
  where
    xs' = Map.mapKeys f . fmap (mapMoves f) $ xs
