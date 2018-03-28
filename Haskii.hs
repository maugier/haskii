{-# LANGUAGE OverloadedStrings #-}

module Haskii where

import Control.Monad.Writer
import Control.Applicative
import Data.Monoid
import Data.String
import Haskii.Types
import Haskii.Internal
import Prelude hiding (length)

move :: (Int,Int) -> Render ()
move (y,x) = Render (tell (Sum y, Sum x))

moveDown, moveRight, moveLeft, moveUp :: Int -> Render ()
moveDown y = move (y,0)
moveRight x = move (0,x)
moveLeft = moveRight . negate
moveUp = moveDown . negate

drawAt :: (Int,Int) -> t -> Render t
drawAt (y,x) t = Render (writer (t,(Sum y,Sum x)))

centered :: Sliceable t => t -> Render t
centered t = t <$ moveLeft ((length t - 1) `div` 2)

oneOf :: [t] -> Render t
oneOf = foldMap return

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


{-
 -  Koch's curve demo
 -
 -  fractal n mix = return x <|> (choice [(-n,0),(0,n),(n,0),(0,-n)] >>= move >> return (mix x) )
 -  colorPrint8 $ move (20,20) >> fractal 1 (fore red) "*" >>= fractal 3 (fore green) >>= fractal 9 (fore blue)
 -
 -}

