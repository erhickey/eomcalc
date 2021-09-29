{-# LANGUAGE OverloadedStrings #-}

module EomJson.EomJson (generateData) where

import Data.Char (ord)
import Data.Either (fromLeft, fromRight, isLeft, lefts, rights)
import qualified Data.IntSet as S (fromList, member)
import Data.Text (Text)
import qualified Data.Text as T (null, pack, unpack)

import EomJson.Assembler.Skills (assembleSkills)
import EomJson.Assembler.Traits (assembleTraits)
import EomJson.Json.Skill (Skill(..))
import EomJson.Json.Trait (Trait(..))
import EomJson.Parse.SkillDetails (skillDetails)
import EomJson.Parse.SkillsAndTraits (ParsedField, skillAndTraitDescriptions)
import EomJson.Util.Util (compose, replaceSmartQuotes)

-- list of skills and traits generated from parsed data
-- skill and trait descriptions -> skill details -> (skills, traits)
-- returns all parse errors if any parsing fails
generateData :: Text -> Text -> Either [String] ([Skill], [Trait])
generateData descIn detailsIn
  | isLeft detailsParseResult = Left parseErrors
  | null $ rights descParseResult = Left parseErrors
  | otherwise = Right (skills, traits)
  where
    descParseResult = skillAndTraitDescriptions descIn
    detailsParseResult = skillDetails detailsIn
    parseErrors = fromLeft "" detailsParseResult: lefts descParseResult
    parsedDescriptions = filter notBlank . map fixParsedField $ rights descParseResult
    details = fromRight [] detailsParseResult
    skills = assembleSkills parsedDescriptions details
    traits = usedTraits skills $ assembleTraits parsedDescriptions

-- filter out traits that are not used by any skills
usedTraits :: [Skill] -> [Trait] -> [Trait]
usedTraits ss = filter (flip S.member usedTraitIds . traitId)
  where usedTraitIds = S.fromList $ concatMap (\s -> [primaryTrait s, secondaryTrait s]) ss

-- true if Text in ParsedField is not blank
notBlank :: ParsedField -> Bool
notBlank (_, t, _) = not $ T.null t

-- apply fixParsedText to a ParsedField
fixParsedField :: ParsedField -> ParsedField
fixParsedField (n, t, ft) = (n, fixParsedText t, ft)

-- composition of all parsed text fixes
fixParsedText :: (Text -> Text)
fixParsedText = compose [ removeSpecialCharacters, replaceSmartQuotes ]

-- remove ascii characters outside range 32 - 126
removeSpecialCharacters :: Text -> Text
removeSpecialCharacters = T.pack . filter ((\n -> n >= 32 && n <= 126) . ord) . T.unpack
