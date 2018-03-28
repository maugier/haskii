{-|
Module      : Haskii.Rainbow
Description : Rendering over Rainbow chunks
Copyright   : (c) 2018 Maxime Augier
License     : BSD3
Stability   : experimental

Instances and helpers for rendering over the Chunk datatype
provided by the 'Rainbow' module.

-}

module Haskii.Rainbow
    ( RenderMode
    , color0
    , color8
    , color256
    , renderWith
    , putStrWith )
    where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Haskii
import Haskii.Types
import Lens.Simple (view, over)
import Prelude hiding (take,drop,length)
import Rainbow (Chunk, Renderable, chunk)
import Rainbow.Types (yarn)
import qualified Rainbow as R

instance Sliceable a => Sliceable (Chunk a) where
    take = over yarn . take
    drop = over yarn . drop
    length = length . view yarn

instance Paddable a => Paddable (Chunk a) where
    padding = chunk . padding

type RenderMode a = (Chunk a -> [ByteString] -> [ByteString])

color0, color8, color256 :: Renderable a => RenderMode a
color0 = R.toByteStringsColors0
color8 = R.toByteStringsColors8
color256 = R.toByteStringsColors256

-- | Perform the rendering of Rainbow chunks, outputting a
-- | list of ByteString chunks suitable for printing
renderWith :: (Renderable t, Paddable t)
           => RenderMode t  -- ^The color encoder, as defined by Rainbow
           -> Render (Chunk t)
           -> [ByteString]

renderWith mode = concatMap (++[ BS.singleton 10 ])
                . map (R.chunksToByteStrings mode)
                . renderChunks


-- | Equivalent to passing the output of 'renderWith' to putStr
putStrWith :: (Renderable t, Paddable t)
          => RenderMode t
          -> Render (Chunk t)
          -> IO ()
putStrWith = (mapM_ BS.putStr .) . renderWith
