{-# LANGUAGE OverloadedStrings #-}

module Haskii.Figlet.FLF where

import Control.Applicative
import Control.Monad (guard, replicateM, replicateM_)
import qualified Data.ByteString as BS    
import Data.Bits
import Data.Char (chr)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B8 
import Data.Attoparsec.ByteString as A (Parser, skipWhile, string, take, takeTill,
                                        takeWhile1, (<?>))
import Data.Attoparsec.ByteString.Char8 (anyChar, char, decimal, endOfLine, hexadecimal,
                                         isEndOfLine, signed, skipSpace)
import Haskii.Figlet.Types


hdrInt :: Parser Int
hdrInt = skipSpace >> decimal

octal :: Parser Int
octal = BS.foldl' step 0 `fmap` takeWhile1 isDigit_w8
  where step a w = a * 8 + fromIntegral (w - 48)
        isDigit_w8 w = (w - 48) <= 9

charCode :: Parser Char
charCode = chr <$> signed (("0x" >> hexadecimal) <|>
                           ("0" >> octal)        <|>
                           (decimal)              )

skipLine :: Parser ()
skipLine = skipWhile (not . isEndOfLine) >> endOfLine

oldLayoutP :: Parser Mode
oldLayoutP = do
    skipSpace
    v <- signed decimal :: Parser Int
    return $ case v of
        -1 -> FullSize
        0 -> Kerning
        n -> Smushing [ rule | (c,rule) <- zip [1..] [EqualCharacter .. BigX]
                             , testBit n c ]

fullLayoutP :: Parser (Mode,Mode)
fullLayoutP = do
    skipSpace
    v <- decimal :: Parser Int
    return (FullSize, FullSize) -- TODO Implement this

readCharacter :: (Char -> Maybe Char) -> Int -> Int -> Parser FigletChar
readCharacter rmhs height maxlen =
    do  first <- takeTill isEndOfLine <* endOfLine
        (stop, body) <- case B8.uncons first of
            Nothing -> fail "Empty line inside character"
            Just ok -> return ok

        let width = BS.length body
        guard (width <= maxlen)
            <?> "Character too large"

        let line = A.take width <* (char stop >> endOfLine)
        middle <- replicateM (height-2) line

        last <- A.take width <* (char stop >> char stop >> endOfLine)

        return . map (map rmhs . B8.unpack) $ first : middle ++ [last]

    

tagged :: Parser FigletChar -> Parser (Char,FigletChar)
tagged p = do
            code <- charCode
            skipLine
            cdata <- p
            return (code, cdata)

switchHardBlank hb ' '            = Nothing
switchHardBlank hb  c | hb == c   = Just ' '
                      | otherwise = Just c
                     

mandatoryChars :: String
mandatoryChars = map chr $ [32..126] ++ [196,214,220,228,246,252,223]

flf :: Parser FLF
flf = do
        -- Header starts here
        ("flf2a" <|> "tlf2a")
        hb <- anyChar

        height <- hdrInt
        guard (height > 0)
            <?> "Invalid height value (must be > 0)"

        baseline <- hdrInt
        guard (baseline > 0 && baseline <= height) 
            <?> "invalid baseline value (must be > 0, less than height)"

        maxlen <- hdrInt
        oldl <- oldLayoutP
        comments <- hdrInt
        direction <- optional hdrInt
        layout <- optional fullLayoutP
        cdtCount <- optional hdrInt

        skipLine

        -- skip comments section
        replicateM_ comments skipLine

        let rdc = readCharacter (switchHardBlank hb) height maxlen

        cdata <- zip mandatoryChars <$> many rdc
        extra <- many $ tagged rdc

        return $ FLF baseline
                     oldl
                     direction
                     layout
                     cdtCount
                     (M.fromList $ cdata ++ extra)


