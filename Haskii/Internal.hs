{-# LANGUAGE FlexibleInstances #-}

module Haskii.Internal 
    where

import Control.Monad.Writer
import Data.IntMap as IM
import Haskii.Internal.RangeMap
import Haskii.Types


renderChunks :: Paddable t => Render t -> [[t]]
renderChunks object = paddedScreen 0 lineBuffer where
    inputLines = [(y,[(x,r)]) | (r, (Sum y,Sum x)) <- (runWriterT.runRender) object ]
    lineBuffer = IM.toList . IM.map pad . IM.fromListWith (++) $ inputLines
    paddedScreen y [] = []
    paddedScreen y ((y',l):rest) = replicate (y'-y-1) [] ++ (l : paddedScreen y' rest)
