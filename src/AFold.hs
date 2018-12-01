-- A version of Fold which can be put inside a data structure
-- without triggering impredicative polymorphism.
{-# LANGUAGE RankNTypes #-}
module AFold where

import Control.Lens


newtype AFold s a = AFold
  { unAFold :: Fold s a }

-- all the elements of the first fold,
-- followed by all the elements of the second fold.
instance Semigroup (AFold s a) where
  afold1 <> afold2 = AFold $ \f s
                  -> unAFold afold1 f s
                  *> unAFold afold2 f s

instance Monoid (AFold s a) where
  mempty = AFold $ \_ s -> pure s
