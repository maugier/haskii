{-# LANGUAGE Safe, TypeFamilies, FlexibleInstances #-}
{-|
Module      : Haskii.Types
Description : Haskell Ascii Art
Copyright   : (c) Maxime Augier, 2018
License     : BSD3
Maintainer  : max@xolus.net
Stability   : experimental

Core definitions, including the Render monad

-}

module Haskii.Types where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad
import Data.String
import Data.Monoid
import Data.Semigroup

-- | The core datatype of this library. It consists of a Writer monad,
-- used to sequence relative movements in a 2d coordinate system,
-- and a List monad for combining multiple layers of drawing.
--
-- This is not a stateful monad; sequencing Renders with `(>>)`
-- does not combine the drawings. Use the `Monoid` instance for that.
--
-- Rather, you can bind monadic functions to split an abstract object
-- into several smaller components, relative to a common reference position.
-- Once you've split your abstract drawing into chunks of strings or text,
-- you can perform the actual rendering.
newtype Render t = Render { runRender :: WriterT (Sum Int,Sum Int) [] t }

-- | Unwraps the Render newtype to a lazy list of chunks with positions.
toChunks :: Render t -> [(t,(Int,Int))]
toChunks = map (\(t,(Sum y,Sum x)) -> (t,(y,x))) . runWriterT . runRender

-- | Wraps a list of chunks into a Render.
fromChunks :: [(t,(Int,Int))] -> Render t
fromChunks = Render . WriterT . map (\(t,(y,x)) -> (t,(Sum y,Sum x)))

instance Show t => Show (Render t) where
    show = ("fromChunks " ++) . show . toChunks

instance Functor Render where
    fmap f (Render m) = Render (fmap f m)

instance Applicative Render where
    pure = Render . pure
    Render a <*> Render b = Render (a <*> b)

instance Monad Render where
    return = Render . return
    Render a >>= f = Render (a >>= runRender . f)

instance Alternative Render where
    empty = Render empty
    Render a <|> Render b = Render (a <|> b)


-- | `mplus` obeys the left-identity law, not the left-catch law
instance MonadPlus Render where
    mzero = empty
    mplus = (<|>)

instance Semigroup (Render t) where
    (<>) = (<|>)

instance Monoid (Render t) where
    mempty = empty
    mappend = (Data.Semigroup.<>)

-- | A class for List-like types, that can be efficiently cut at the start or end
-- with take or drop.
class Sliceable t where
    take :: Int -> t -> t
    drop :: Int -> t -> t
    length :: t -> Int

-- | When working with lists, which element should be used by the Paddable instance.
-- | Basically a hack to work around OverlappingInstances.
class Blank t where
    blank :: t

instance Blank Char where
    blank = ' '

-- | A datatype for which we know how to generate padding of a given length.
class Sliceable t => Paddable t where
    padding :: Int -> t

-- | A datatype that we know how to break efficiently in chunks over a predicate.
-- the removed parts will become transparent.
class Sliceable t => Transparent t where
    type Elem t
    breakTransparent :: (Elem t -> Bool) -> t -> (t,t)

instance Sliceable [t] where
    take = Prelude.take
    drop = Prelude.drop
    length = Prelude.length

instance Eq t => Transparent [t] where
    type Elem [t] = t
    breakTransparent = Prelude.break

instance (Eq t, Blank t) => Paddable [t] where
    padding = flip Prelude.replicate blank

instance IsString t => IsString (Render t) where
    fromString = return . fromString


