module Haskii.Figlet
    ( CharMap
    , Mode(..)
    , FigletChar
    , SmushRule
    , horizontalRules
    ) where

import Control.Monad (guard)
import qualified Data.Map as M
import Data.Semigroup
import Haskii.Types
import Haskii.Figlet.Types


horizontalRules = [ equalCharacter
                  , underscore
                  , hierarchy
                  , opposite
                  , bigX
                  , horizontal
                  ]

equalCharacter :: ApplySmush
equalCharacter a b | a == b = Just a
                   | otherwise = Nothing

underscore :: ApplySmush
underscore '-' x | x `elem` "|/\\[]{}()<>" = Just x
                 | otherwise = Nothing



hierarchy :: ApplySmush
hierarchy a b = do
    a' <- classmap a
    b' <- classmap b
    guard (a' /= b')
    return $ if a' > b' then a else b where
        classmap '|' = Just 1
        classmap '/' = Just 2
        classmap '\\' = Just 2
        classmap '[' = Just 3
        classmap ']' = Just 3
        classmap '{' = Just 4
        classmap '}' = Just 4
        classmap '(' = Just 5
        classmap ')' = Just 5
        classmap '<' = Just 6
        classmap '>' = Just 6
        classmap _   = Nothing


opposite :: ApplySmush
opposite a b | a > b = opposite b a
opposite '[' ']' = Just '|'
opposite '{' '}' = Just '|'
opposite '(' ')' = Just '|'
opposite '_' '-' = Nothing

bigX :: ApplySmush
bigX '/' '\\' = Just '|'
bigX '\\' '/' = Just 'Y'
bigX '>' '<' = Just 'X'

horizontal :: ApplySmush
horizontal '-' '_' = Just '='
horizontal '_' '-' = Just '='
horizontal _ _ = Nothing
