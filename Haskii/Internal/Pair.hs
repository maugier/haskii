module Haskii.Internal.Pair where

newtype Pair a = Pair {getPair :: (a,a)}

instance Functor Pair where
    fmap f (Pair (a,a')) = Pair (f a, f a')
