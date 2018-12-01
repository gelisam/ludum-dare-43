{-# LANGUAGE DefaultSignatures, FlexibleInstances, TypeSynonymInstances #-}
module Debug where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map


type Indent = Int

class Debug a where
  printIndented :: Indent -> a -> IO ()

  default printIndented :: Show a => Indent -> a -> IO ()
  printIndented indent x = do
    putStr $ replicate (indent  * 2) ' '
    print x

debug :: Debug a
      => a -> IO ()
debug = printIndented 0

putIndentedStrLn :: Indent -> String -> IO ()
putIndentedStrLn indent s = do
  putStr $ replicate (indent  * 2) ' '
  putStrLn s

instance Debug ()
instance Debug Double
instance Debug Int
instance {-# OVERLAPS #-} Debug String

instance Debug a => Debug [a] where
  printIndented indent [] = putIndentedStrLn indent "[]"
  printIndented indent (x:xs) = do
    putIndentedStrLn indent "["
    printIndented (succ indent) x
    for_ xs $ \x' -> do
      putIndentedStrLn indent ","
      printIndented (succ indent) x'
    putIndentedStrLn indent "]"


instance (Debug a, Debug b) => Debug (a, b) where
  printIndented indent (a, b) = do
    putIndentedStrLn indent "("
    printIndented (succ indent) a
    putIndentedStrLn indent ","
    printIndented (succ indent) b
    putIndentedStrLn indent ")"

instance (Debug k, Debug a) => Debug (Map k a) where
  printIndented indent map_ = case Map.toList map_ of
    [] -> putIndentedStrLn indent "{}"
    (k,x):kxs -> do
      putIndentedStrLn indent "{"
      printIndented (succ indent) k
      putIndentedStrLn (succ indent) ":"
      printIndented (succ indent) x
      for_ kxs $ \(k',x') -> do
        putIndentedStrLn indent ","
        printIndented (succ indent) k'
        putIndentedStrLn (succ indent) ":"
        printIndented (succ indent) x'
      putIndentedStrLn indent "}"


newtype SingleLine a = SingleLine
  { unSingleLine :: a }
  deriving (Eq, Ord, Show)

instance Show a => Debug (SingleLine a) where
  printIndented indent (SingleLine a) = putIndentedStrLn indent (show a)
