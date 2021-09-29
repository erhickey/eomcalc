{-# LANGUAGE OverloadedStrings #-}

module EomJson.Parse.SkillsAndTraits (FieldType(..), ParsedField, skillAndTraitDescriptions) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T (concat, lines, pack)

import Data.Attoparsec.Text (anyChar, decimal, manyTill, parseOnly, Parser, string)

data FieldType = SkillName | SkillDescription | TraitName | TraitDescription | TraitMods deriving (Eq)
type ParsedField = (Int, Text, FieldType)

-- attempt to parse skill and trait descriptions
descriptionLinesParser :: Parser ParsedField
descriptionLinesParser =
      skillNameParser
  <|> skillDescriptionParser
  <|> traitNameParser
  <|> traitDescriptionParser
  <|> traitModsParser

-- attempt to parse field
descriptionLineParser :: Text -> FieldType -> Parser ParsedField
descriptionLineParser s f = do
  _ <- string (T.concat ["\t[\"", s, "_"])
  sId <- decimal
  _ <- string "\"] = \""
  field <- manyTill anyChar (string "\",")
  pure (sId, T.pack field, f)

skillNameParser :: Parser ParsedField
skillNameParser = descriptionLineParser "SkillName" SkillName

skillDescriptionParser :: Parser ParsedField
skillDescriptionParser = descriptionLineParser "SkillTips" SkillDescription

traitNameParser :: Parser ParsedField
traitNameParser = descriptionLineParser "TraitName" TraitName

traitDescriptionParser :: Parser ParsedField
traitDescriptionParser = descriptionLineParser "TraitTip" TraitDescription

traitModsParser :: Parser ParsedField
traitModsParser = descriptionLineParser "TraitTips" TraitMods

-- skill and trait descriptions parsed from input
skillAndTraitDescriptions :: Text -> [Either String ParsedField]
skillAndTraitDescriptions = map (parseOnly descriptionLinesParser) . T.lines
