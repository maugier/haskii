{-# LANGUAGE Safe,FlexibleInstances #-}

{-|
 - Module      : Haskii.Internal.RangeMap
 - Description : A Map using non-overlapping ranges as keys
 - Copyright   : (c) Maxime Augier, 2018
 - License     : BSD3
 - Maintainer  : max@xolus.net
 - Stability   : experimental
 -
 - This datastructure is used to keep track of covering chunks for a single line of text.
 -
 - -}

module Haskii.Internal.RangeMap
    ( RangeMap
    , empty
    , fromList
    , insert
    , pad
    , singleton
    , squash
    , toList
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Ord
import Haskii.Types
import Prelude hiding (span, take, drop, length)

-- | An interval [a,b[ over integers. 
data Span = Span Int Int
    deriving (Show)
span :: (Int,Int) -> Span
span (a,b) | a > b = Span b a
           | otherwise = Span a b

-- | Two intervals are only considered different if they are non-overlapping.
-- | This ensures that a map keyed by intervals cannot contain overlapping ranges.
instance Ord Span where
    compare (Span a b) (Span a' b') = case (b <= a', a >= b') of
        (True,_) -> LT
        (_,True) -> GT
        _        -> EQ

instance Eq Span where
    a == b = (compare a b == EQ)

-- | The main datatype. Conceptually equivalent to [((Int,Int), r)], with non-overlapping intervals as keys
newtype RangeMap r = RangeMap (Map Span (Span,r))

-- | An empty RangeMap
empty :: RangeMap r
empty = RangeMap (M.empty)

-- | A RangeMap containing a single Sliceable element, at a given offset. The second bound
-- | is calculated from the length of the element.
singleton :: Sliceable r => Int -> r -> RangeMap r
singleton offset r = insert offset r empty

-- | Insert a new element at the given range. If there is overlap with existing keys,
-- | the existing intervals will be sliced to remove the conflicting parts.
insert :: Sliceable r => Int -> r -> RangeMap r -> RangeMap r
insert offset r = insert' (Span offset (offset + length r), r)

insert' :: Sliceable r => (Span,r) -> RangeMap r -> RangeMap r
insert' e@(s,_) (RangeMap m) =
    case M.lookup s m of
        Nothing -> RangeMap (M.insert s e m)
        Just e'@(s',_) -> L.foldr (insert') (RangeMap (M.delete s' m)) (breakApart e e')


-- | Resolves a conflict between two sliceables, turning them into one, two or three
-- | non-overlapping chunks.
breakApart :: Sliceable r => (Span,r) -> (Span,r) -> [(Span,r)]
breakApart x@((Span a b),r) y@((Span a' b'),r') = 
    let
        leftoverL = (Span a' a, take (a-a') r')
        leftoverR = (Span b b', drop (b-a') r')
    in case (a <= a', b >= b') of
        (True,True)   -> [x]
        (True,False)  -> [x, leftoverR]
        (False,True)  -> [leftoverL, x]
        (False,False) -> [leftoverL, x, leftoverR] where

-- | Builds a RangeMap from a list of elements with offsets. The upper interval bound is computed from 
-- | the element length. In case of conflicts, the later elements cover the earlier ones.
fromList :: Sliceable r => [(Int,r)] -> RangeMap r
fromList = L.foldr (uncurry insert) empty

-- | Turn back the RangeMap into a map.
-- | `fromList . toList == id`
-- | `(toList . fromList) l == l iif. l does not contain any overlapping ranges.
toList :: RangeMap r -> [(Int,r)]
toList (RangeMap m) = [(a,r) | (Span a b, r) <- M.elems m ]

-- | Turns a list of possibly overlapping chunks into non-overlapping ones, cutting the covered pieces if needed.
squash :: Sliceable r => [(Int,r)] -> [(Int,r)]
squash = toList . fromList

-- | Turns a list of chunks with explicit offsets into a list of contiguous chunks, generating padding chunks if needed.
pad :: Paddable r => [(Int,r)] -> [r]
pad = pad' 0 . squash where
    pad' _ [] = []
    pad' n ((o,x):xs) = let rest = x : pad' (o + length x) xs in if (o-n) > 0 then padding (o-n) : rest else rest

instance Show r => Show (RangeMap r) where
    show rm = "fromList " ++ show (toList rm)
