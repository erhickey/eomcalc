{-# LANGUAGE OverloadedStrings #-}

module EomJson.Parse.TraitDetails (TraitDetail(..), traitDetails) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Data.Attoparsec.Text (anyChar, char, decimal, many', manyTill, parseOnly, Parser, string, takeTill)

data TraitDetail = TraitDetail
  { traitDetailId :: Int
  , traitDetailBreakpoints :: [Int]
  } deriving (Show)

-- attempt to parse trait details
traitDetailsParser :: Parser [TraitDetail]
traitDetailsParser = catMaybes <$> many' (pure <$> traitDetailParser <|> (discardLine >> pure Nothing))

-- attempt to parse details of a single trait
-- there is no guarantee the input won't change in future game updates
-- chose a naive implementation for this reason, it may be completely replaced if the input changes greatly
traitDetailParser :: Parser TraitDetail
traitDetailParser = do
  _ <- string "\t\tID = "
  tId <- decimal
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tTraitBuffs = {")
  bs <- many' traitBreakpointParser
  pure (TraitDetail tId bs)

-- parse breakpoints of a trait
traitBreakpointParser :: Parser Int
traitBreakpointParser = do
  _ <- string "{num=" <|> string ",{num="
  bp <- decimal
  _ <- manyTill anyChar (char '}')
  pure bp

-- discard rest of line
discardLine :: Parser Text
discardLine = takeTill ('\n' ==) <* char '\n'

-- trait details parsed from input
traitDetails :: Text -> Either String [TraitDetail]
traitDetails = parseOnly traitDetailsParser
