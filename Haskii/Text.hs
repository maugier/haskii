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
-- | under each other, left-aligning their start location
multiline :: Text -> Render Text
multiline t = do
    (c,l) <- oneOf . zip [0..] . T.lines $ t
    drawAt (c,0) l

-- | Perform the rendering operation and produce a single output
-- | suitable for printing directly.
render :: Render Text -> Lazy.Text
render = Lazy.unlines . map Lazy.fromChunks . renderChunks

-- | Shortcut for printing the rendered output on stdout.
putStrLn :: Render T.Text -> IO ()
putStrLn = Lazy.putStrLn . render
