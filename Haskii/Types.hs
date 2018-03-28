{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haskii.Types where

import Control.Applicative
import Control.Monad.Writer
import Data.String
import Data.Monoid
import Data.Semigroup

-- | A collection of drawable objects of type `t`, with assorted 2d coordinates
newtype Render t = Render { runRender :: WriterT (Sum Int,Sum Int) [] t }
    deriving (Functor, Applicative, Alternative, Monad)
    

instance Semigroup (Render t) where
    (<>) = (<|>)

instance Monoid (Render t) where
    mempty = empty
    mappend = (Data.Semigroup.<>)

class Sliceable t where
    take :: Int -> t -> t
    drop :: Int -> t -> t
    length :: t -> Int

class Sliceable t => Paddable t where
    padding :: Int -> t

instance Sliceable [t] where
    take = Prelude.take
    drop = Prelude.drop
    length = Prelude.length

instance Monoid t => Paddable [t] where
    padding = flip Prelude.replicate mempty

instance IsString t => IsString (Render t) where
    fromString = return . fromString


