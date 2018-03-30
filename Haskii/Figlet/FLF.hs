{-# LANGUAGE OverloadedStrings #-}

module Haskii.Figlet.FLF where

import Control.Applicative
import Control.Monad (guard)
import qualified Data.ByteString as BS    
import Data.Bits
import Data.ByteString.Char8(pack)
import Data.Attoparsec.ByteString (Parser, string)
import Data.Attoparsec.ByteString.Char8 (anyChar, decimal, signed, takeWhile1)
import Haskii.Figlet


data Header = Header {
    hardblank :: Char,
    height :: Int,
    baseline :: Int,
    maxLength :: Int,
    oldLayout :: Int,
    commentLines :: Int,
    printDirection :: Int,
    fullLayout :: Int,
    codetagCount :: Int
}

data FLF = FLF Header

hdrInt :: Parser Int
hdrInt = skipSpace >> decimal

lineP :: Parser ()
lineP = skipWhile (not . isEndOfLine) >> endOfLine

oldLayoutP :: Parser Mode
oldLayoutP = do
    skipSpace
    v <- signed
    return $ case v of
        -1 -> FullSize
        0 -> Kerning
        n -> Smushing [ rule | (c,rule) <- zipWith [1..] horizontalRules
                             , testBit n c ]

fullLayoutP :: Parser Int
fullLayoutP = _

flfHeader :: Parser Header
flfHeader = do
                "flf2a" 
                hb <- anyChar
                height <- hdrInt
                baseline <- hdrInt
                
                guard (baseline > 0 && baseline <= height) 
                    <?> "invalid baseline value"

                maxlen <- hdrInt
                oldl <- oldLayoutP
                comments <- hdrInt
                direction <- hdrInt
                layout <- fullLayoutP
                cdtCount <- hdrInt

                lineP                
                
                return undefined
    

flfFile :: Parser FLF
    header <- flfHeader
    replicateM_ (commentLines header) lineP

