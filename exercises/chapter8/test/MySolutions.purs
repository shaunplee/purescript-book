module Test.MySolutions where

import Prelude
import Data.Array (concat, foldM, head, nub, sort, tail)
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
third :: forall a. Array a -> Maybe a
third xs = do
  xs' <- tail xs
  xs'' <- tail xs'
  head xs''

possibleSums :: Array Int -> Array Int
possibleSums cs = sort $ nub $ foldM (\acc c -> [ acc, acc + c ]) 0 cs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil

filterM p (Cons x xs) = do
  px <- p x
  if px then
    Cons x <$> filterM p xs
  else
    filterM p xs
