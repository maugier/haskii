module Haskii.Figlet
    ( Mode
    , SmushRule
    , horizontalRules
    ) where

import Control.Monad (guard)
import qualified Data.Map as M
import Data.Semigroup
import Haskii.Types

type SmushRule = (Char -> Char -> Maybe Char)

data Mode = FullSize | Kerning | Smushing [SmushRule]

data Layout = Layout { horizontalMode :: Mode
                     , verticalMode :: Mode
                     }
                
type FigletChar = Render String

type CharMap = M.Map Char FigletChar

horizontalRules = [ equalCharacter
                  , underscore
                  , hierarchy
                  , opposite
                  , bigX
                  , horizontal
                  ]

equalCharacter :: SmushRule
equalCharacter a b | a == b = Just a
                   | otherwise = Nothing

underscore :: SmushRule
underscore '-' x | x `elem` "|/\\[]{}()<>" = Just x
                 | otherwise = Nothing



hierarchy :: SmushRule
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


opposite :: SmushRule
opposite a b | a > b = opposite b a
opposite '[' ']' = Just '|'
opposite '{' '}' = Just '|'
opposite '(' ')' = Just '|'
opposite '_' '-' = Nothing

bigX :: SmushRule
bigX '/' '\\' = Just '|'
bigX '\\' '/' = Just 'Y'
bigX '>' '<' = Just 'X'

horizontal :: SmushRule
horizontal '-' '_' = Just '='
horizontal '_' '-' = Just '='
horizontal _ _ = Nothing
