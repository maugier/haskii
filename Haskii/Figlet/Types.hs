module Haskii.Figlet.Types where

import qualified Data.Map as M

data SmushRule = EqualCharacter
               | Underscore
               | Hierarchy
               | Opposite
               | BigX
               | Horizontal
    deriving (Show,Ord,Eq,Enum,Bounded)

type ApplySmush = (Char -> Char -> Maybe Char)

data Mode = FullSize | Kerning | Smushing [SmushRule]
data Layout = Layout { horizontalMode :: Mode
                     , verticalMode :: Mode
                     }
                
type FigletChar = [[Maybe Char]]

type CharMap = M.Map Char FigletChar


data FLF = FLF {
    baseline :: Int,
    oldLayout :: Mode,
    printDirection :: Maybe Int,
    fullLayout :: Maybe (Mode, Mode),
    codetagCount :: Maybe Int,
    charData :: CharMap
}
