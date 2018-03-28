{-# LANGUAGE Safe #-}
{-|
 - Module      : Haskii.Types
 - Description : Haskell Ascii Art
 - Copyright   : (c) Maxime Augier, 2018
 - License     : BSD3
 - Maintainer  : max@xolus.net
 - Stability   : experimental
 -
 - Core definitions, including the Render monad
 -
 - -}

module Haskii.Types where

import Control.Applicative
import Control.Monad.Writer
import Control.Monad
import Data.String
import Data.Monoid
import Data.Semigroup

-- | A collection of drawable objects of type `t`, with assorted 2d coordinates.
newtype Render t = Render { runRender :: WriterT (Sum Int,Sum Int) [] t }

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


-- | `mplus` combines rendering of several elements.
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
class Transparent t where
    transparent :: t

instance Transparent Char where
    transparent = ' '

-- | A datatype for which we know how to generate padding of a given length.
class Sliceable t => Paddable t where
    padding :: Int -> t

instance Sliceable [t] where
    take = Prelude.take
    drop = Prelude.drop
    length = Prelude.length

instance Transparent t => Paddable [t] where
    padding = flip Prelude.replicate transparent

instance IsString t => IsString (Render t) where
    fromString = return . fromString


