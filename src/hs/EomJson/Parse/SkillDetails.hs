{-# LANGUAGE OverloadedStrings #-}

module EomJson.Parse.SkillDetails (SkillDetail(..), skillDetails) where

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Text (Text)

import Data.Attoparsec.Text (char, decimal, double, many', manyTill, parseOnly, Parser, string, takeTill)

data SkillDetail = SkillDetail
  { detailId :: Int
  , detailRarity :: Int
  , detailSkillType :: Int
  , detailIsActive :: Bool
  , detailCooldown :: Double
  , detailPrimaryTrait :: Int
  , detailSecondaryTrait :: Int
  } deriving (Show)

-- attempt to parse skill details
skillDetailsParser :: Parser [SkillDetail]
skillDetailsParser = catMaybes <$> many' (pure <$> skillDetailParser <|> (discardLine >> pure Nothing))

-- attempt to parse details of a single skill
-- there is no guarantee the input won't change in future game updates
-- chose a naive implementation for this reason, it may be completely replaced if the input changes greatly
skillDetailParser :: Parser SkillDetail
skillDetailParser = do
  _ <- string "\t\tID = "
  sId <- decimal
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tQuality = ")
  qual <- decimal
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tSkillType = ")
  sType <- decimal
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tIsActive = ")
  active <- decimal
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tCD = ")
  cd <- double
  _ <- char ','
  _ <- manyTill discardLine (string "\t\tCareerTrait = {")
  pTrait <- decimal
  _ <- string "},"
  _ <- manyTill discardLine (string "\t\tElementTrait = {")
  sTrait <- decimal
  _ <- string "},"
  pure (SkillDetail sId qual sType (active == (1 :: Integer)) cd pTrait sTrait)

-- discard rest of line
discardLine :: Parser Text
discardLine = takeTill ('\n' ==) <* char '\n'

-- skill details parsed from input
skillDetails :: Text -> Either String [SkillDetail]
skillDetails = parseOnly skillDetailsParser
