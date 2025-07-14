module Lib where

import Data.Attoparsec.Text

import Data.Map.Strict qualified as Map

import Control.Applicative (Alternative (..), optional, some)
import Control.Applicative.Combinators (someTill)
import Control.Monad (void)
import Data.Char (isAlphaNum)
import Data.Either (rights)
import Data.Hashable (Hashable)
import Data.List.Extra (trim)
import Data.Map.Strict (Map)
import Data.String (fromString)
import Data.Text (Text)

newtype Header = Header Text
    deriving stock (Show)
    deriving newtype (Eq, Ord, Hashable)

data Assignment = Assignment
    { key :: Text
    , value :: Text
    }
    deriving stock (Show, Eq)

newtype IniConfig = IniConfig (Map Header [Assignment])
    deriving stock (Show)
    deriving newtype (Eq)

newtype Section = Section {unSection :: (Header, [Assignment])}
    deriving stock (Show)
    deriving newtype (Eq)

parseHeader :: Parser Header
parseHeader = do
    char '['
    headerTxt <- some parseHeaderText
    char ']'
    skipEOL
    pure . Header . fromString $ headerTxt
  where
    parseHeaderText :: Parser Char
    parseHeaderText = noneOf "[]\n"

parseAssignment :: Parser Assignment
parseAssignment = do
    key <- fromString . trim <$> some parseAssignmentKey
    parseEqSign
    value <- parseAssignmentVal
    skipEOL
    pure $ Assignment{key, value}
  where
    parseAssignmentKey :: Parser Char
    parseAssignmentKey = noneOf "\n="

    parseEqSign :: Parser ()
    parseEqSign = void $ (skipSpace *> char '=' <* skipSpace) <|> (skipSpace *> char '=') <|> (char '=' <* skipSpace) <|> char '='

    parseAssignmentVal :: Parser Text
    parseAssignmentVal = fromString <$> parseLine
      where
        parseLine = parseMultiLine <|> parseSingleLine
        parseSingleLine = manyTill anyChar endOfLine <|> many (noneOf "\n")
        parseMultiLine = do
            multiLines <- concatMap (<> "\\\n") <$> some (someTill (noneOf "\\\n") (string "\\\n" <* skipEOL))
            finalLine <- parseSingleLine
            pure $ multiLines <> finalLine

parseSection :: Parser Section
parseSection = do
    header <- parseHeader
    assignments <- fmap rights $ many $ do
        eitherP skipComment parseAssignment
    skipEOL
    pure . Section $ (header, assignments)

parseIniConfig :: Parser IniConfig
parseIniConfig = do
    skipEOL
    optional skipComment
    sections <- many parseSection
    pure . IniConfig . Map.fromList . map (.unSection) $ sections

skipComment :: Parser ()
skipComment = do
    char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

skipEOL :: Parser ()
skipEOL = skipMany endOfLine

alphaNum :: Parser Char
alphaNum = satisfy (liftA2 (||) isAlphaNum (inClass "."))

noneOf :: String -> Parser Char
noneOf x = satisfy (notInClass x)
