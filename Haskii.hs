{-# LANGUAGE Safe, OverloadedStrings #-}

{-|
Module      : Haskii
Description : Haskell Ascii Art
Copyright   : (c) Maxime Augier, 2018
License     : BSD3
Maintainer  : max@xolus.net
Stability   : experimental

The main, backend-agnostig Haskii module

-}

module Haskii
    (
    -- * The Render monad 
    Render()
    , move
    , drawAt
    , fromChunks
    , toChunks
    , oneOf
    -- * Performing rendering
    , renderChunks
    -- * Drawing primitives
    , moveDown
    , moveLeft
    , moveRight
    , moveUp
    , block
    , centered
    , line
    , shadow
    , box
    , boxed
    , boundingBox
    ) where

import Control.Monad.Writer
import Control.Applicative
import Data.Monoid
import Data.Semigroup (Min(..),Max(..),sconcat)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.IntMap as IM
import Data.String
import Haskii.Types
import Haskii.Internal.RangeMap (pad)
import Prelude hiding (length)

-- | Move the cursor location by a relative location
move :: (Int,Int) -- ^ A (y,x) pair. y is the vertical axis (pointing down), x the horizontal one (pointing right)
     -> Render ()
move (y,x) = Render (tell (Sum y, Sum x))

-- | Shortcuts for moving along a single axis

moveDown, moveRight, moveLeft, moveUp :: Int -> Render ()
moveDown y = move (y,0)
moveRight x = move (0,x)
moveLeft = moveRight . negate
moveUp = moveDown . negate

-- | Equivalent to
--  
-- > move (y,x) >> return t
drawAt :: (Int,Int) -> t -> Render t
drawAt (y,x) t = Render (writer (t,(Sum y,Sum x)))


-- | Combine several objects for rendering. Equivalent to:
--
-- > foldMap return
--
-- Examples:
--
-- >>> renderChunks $ oneOf [1,2,3,4] >>= (\x -> move(x,x)) >>  return "hello"
-- [[" ","hello"],["  ","hello"],["   ","hello"],["    ","hello"]]
--
-- >>> printChunks $ oneOf [1,2,3,4] >>= (\x -> move(x,x)) >>  return "hello"
--  hello
--   hello
--    hello
--     hello
oneOf :: [t] -> Render t
oneOf = foldMap return

-- | Draw a path across several points.
-- The given coordinates are always relative to the current position.
-- It can only draw horizontal, vertical, or diagonal lines. It is up to the caller
-- to ensure that contiguous points in the list are properly aligned.
--
-- >>> printChunks $ line [(0,0),(5,5),(5,10),(3,10),(3,0)]
-- +
--  \
--   \
-- +---------+
--     \     |
--      +----+
line :: IsString t => [(Int,Int)] -> Render t
line [] = mempty
line [p] = "+" <$ move p
line ((y,x):(r@((y',x'):_))) = (move (y,x) >> ("+" <|> drawSegment (y'-y) (x'-x))) <|> line r

drawSegment :: IsString t => Int -> Int -> Render t
drawSegment 0 0 = mempty
drawSegment 0 x | x > 0  = drawAt (0,1) . fromString $ replicate (x-1) '-'
                | x < 0  = moveRight x >> drawSegment 0 (-x)
drawSegment y 0 | y > 0  = oneOf [1..(y-1)] >>= moveDown >> "|"
                | y < 0  = moveDown y >> drawSegment (-y) 0
drawSegment x y | y < 0  = move (x,y) >> drawSegment (-x) (-y)
                | x == y = oneOf [1..(y-1)] >>= (\y -> move (y,y)) >> "\\"
                | x == -y = oneOf [1..(y-1)] >>= (\y -> move (y,-y)) >> "/"
                | otherwise = error $ show (x,y) ++ " is not a supported direction for line segments"


-- | Makes a multiline block, going one step down after each line.
--
-- >>> printChunks $ (move (2,2) >> block ["Hello","Haskell","World","!"])
-- <BLANKLINE>
-- <BLANKLINE>
--   Hello
--   Haskell
--   World
--   !
block :: [t] -> Render t
block xs = mconcat [ drawAt (line,0) x | (line,x) <- zip [0..] xs ]

-- | For a single text of chunk of a known length, center it around the current location
--
-- >>> printChunks $ moveRight 8 >> block [ "Hello", "Haskell", "World!", ":)"] >>= centered
--       Hello
--      Haskell
--       World!
--         :)
centered :: Sliceable t => t -> Render t
centered t = t <$ moveLeft ((length t - 1) `div` 2)

shadow :: (t -> t) -> t -> Render t
shadow style contents = (drawAt (-1,-1) $ style contents) <> return contents

-- | Computes the bounding box of a Render
--
-- >>> boundingBox $ move (100, 100) >> block ["Hello","World"]
-- ((100,100),(102,105))
boundingBox :: Sliceable t => Render t -> ((Int,Int),(Int,Int))
boundingBox t = case nonEmpty (toChunks t) of
                    Nothing -> ((0,0),(0,0))
                    Just chunks -> let (Min ymin, Min xmin, Max ymax, Max xmax) = 
                                        sconcat . fmap (\(t,(y,x)) -> (Min y, Min x, Max (y+1), Max (x+length t))) $ chunks
                                   in ((ymin,xmin),(ymax,xmax))

-- | Draw a solid box around a Render, without the Render itself. Useful if
-- you need to give a different style to the box.
-- Examples:
-- printChunks $ 
box :: (IsString t, Sliceable t) => Render t -> Render t
box t = let ((y1,x1),(y2,x2)) = boundingBox t 
         in line [(y1-1,x1-1),(y1-1,x2),(y2,x2),(y2,x1-1),(x1-1,y1-1)]


-- | Draw a render with a solid bounding box around.
-- Equivalent to @box t <> t@
--
-- >>> printChunks $ boxed ((move (4,7) >> boxed (boxed (return "Haskell"))) <> (move (2,2) >> boxed (return "Hello")))
-- +---------------+
-- |+-----+        |
-- ||Hello|-------+|
-- |+-----+------+||
-- |    ||Haskell|||
-- |    |+-------+||
-- |    +---------+|
-- +---------------+
boxed :: (IsString t, Sliceable t) => Render t -> Render t
boxed t = box t <> t

-- | Split a chunk in order to remove transparent parts matching a predicate
transparent :: Transparent t => (Elem t -> Bool) -> t -> [(Int,t)]
transparent pred = trans' 0 where
    trans' a xs = let (blanks, rest) = breakTransparent (not.pred) xs
                      (nonblanks, rest') = breakTransparent (pred) rest
                      lb = length blanks
                      lnb = length nonblanks
                  in (lb, nonblanks) : trans' (lb+lnb) rest'

-- | Actually performs the rendering, by slicing and padding the chunks in the Render
-- to output lists of contiguous chunks
--
-- >>> renderChunks $ (drawAt (3,3) "------------") <> (drawAt (3,5) "Hello") <> (drawAt (1,1) "test")
-- [[" ","test"],[],["   ","--","Hello","-----"]]

renderChunks :: Paddable t => Render t -> [[t]]
renderChunks object = paddedScreen 0 lineBuffer where
    inputLines = [(y,[(x,r)]) | (r, (Sum y,Sum x)) <- (runWriterT.runRender) object ]
    lineBuffer = IM.toList . IM.map pad . IM.fromListWith (++) $ inputLines
    paddedScreen y [] = []
    paddedScreen y ((y',l):rest) = replicate (y'-y) [] ++ (l : paddedScreen (y'+1) rest)

-- | A shortcut to write a Render String directly to stdout
printChunks :: Render String -> IO ()
printChunks = mapM_ (putStrLn . concat) . renderChunks

{-
 -  Koch's curve demo
 -
 -  fractal n mix = return x <|> (choice [(-n,0),(0,n),(n,0),(0,-n)] >>= move >> return (mix x) )
 -  colorPrint8 $ move (20,20) >> fractal 1 (fore red) "*" >>= fractal 3 (fore green) >>= fractal 9 (fore blue)
 -
 -}

