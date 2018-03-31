{-# Language FlexibleInstances, TypeFamilies #-}

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
    , modeBW
    , mode8c
    , mode256c
    , renderWith
    , putStrWith
    , bow )
    where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.String (IsString, fromString)
import Haskii hiding (shadow)
import Haskii.Types
import Haskii.Internal.Pair
import Lens.Simple (view, over, set)
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

instance Transparent a => Transparent (Chunk a) where
    type Elem (Chunk a) = Elem a
    breakTransparent pred = getPair . yarn (Pair . breakTransparent pred)
    
instance IsString a => IsString (Chunk a) where
    fromString = chunk . fromString

type RenderMode a = (Chunk a -> [ByteString] -> [ByteString])

modeBW, mode8c, mode256c :: Renderable a => RenderMode a
modeBW = R.toByteStringsColors0
mode8c = R.toByteStringsColors8
mode256c = R.toByteStringsColors256

bow :: [Chunk a -> b] -> (Int,Int) -> Render a -> Render b
bow colors shift = fold . reverse . zipWith (fmap) colors . iterate (move shift >>) . fmap chunk


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
