{-# LANGUAGE Safe, TypeFamilies  #-}

{-|
Module      : Haskii.Text
Description : Rendering over Data.Text
Copyright   : (c) 2018 Maxime Augier
License     : BSD3
Stability   : experimental

Instances and helpers for rendering over Data.Text
-}

module Haskii.Text where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import Haskii
import Haskii.Types

instance Sliceable Text where
    take = T.take
    drop = T.drop
    length = T.length

instance Paddable Text where
    padding n = T.replicate n (T.singleton ' ')

instance Transparent Text where
    type Elem Text = Char
    breakTransparent = T.break

-- | Splits a piece of text into several lines, and render them
-- under each other, left-aligning their start location
multiline :: Text -> Render Text
multiline t = do
    (c,l) <- oneOf . zip [0..] . T.lines $ t
    drawAt (c,0) l

-- | Fill a rectangle of given size, having the upper left corner at the origin.
--
-- Example:
-- >>> Haskii.Text.putStrLn $ oneOf [(1,8,'.'), (3,5,'/'), (6,4,'#')] >>= ( \(p,s,c) -> move (p,p) >> fill c (s,s))
-- <EMPTYLINE>
--  ........
--  ........
--  ../////.
--  ../////.
--  ../////.
--  ..///####
--  ..///####
--  .....####
--       ####

fill :: Char -> (Int,Int) -> Render Text
fill c (height, width) = fromChunks [ (T.replicate width (T.singleton c),(l,0)) | l <- [0..height-1] ]

-- | Perform the rendering operation and produce a single output
-- suitable for printing directly.
render :: Render Text -> Lazy.Text
render = Lazy.unlines . map Lazy.fromChunks . renderChunks

-- | Shortcut for printing the rendered output on stdout.
putStrLn :: Render T.Text -> IO ()
putStrLn = Lazy.putStrLn . render
