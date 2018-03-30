module Haskii.Figlet.Types where

import qualified Data.Map as M
import Data.Text (Text)

data SmushRule = EqualCharacter
               | Underscore
               | Hierarchy
               | Opposite
               | BigX
               | Horizontal
    deriving (Show,Ord,Eq,Enum,Bounded)

type ApplySmush = (Char -> Char -> Maybe Char)

data Mode = FullSize | Kerning | Smushing [SmushRule]
    deriving (Show)

data Layout = Layout { horizontalMode :: Mode
                     , verticalMode :: Mode
                     }
    deriving Show
                
type FigletChar = [(Text,Int)]

type CharMap = M.Map Char FigletChar


data FLF = FLF {
    baseline :: Int,
    oldLayout :: Mode,
    printDirection :: Maybe Int,
    fullLayout :: Maybe (Mode, Mode),
    codetagCount :: Maybe Int,
    charData :: CharMap
} deriving (Show)
