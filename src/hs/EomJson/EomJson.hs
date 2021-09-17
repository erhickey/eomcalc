module EomJson.EomJson (generateData) where

import Data.Either (fromLeft, fromRight, isLeft, lefts, rights)
import Data.IntSet as S (fromList, member)
import Data.Text (Text)

import EomJson.Assembler.Skills (assembleSkills)
import EomJson.Assembler.Traits (assembleTraits)
import EomJson.Json.Skill (Skill(..))
import EomJson.Json.Trait (Trait(..))
import EomJson.Parse.SkillDetails (skillDetails)
import EomJson.Parse.SkillsAndTraits (skillAndTraitDescriptions)

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
    parsedDescriptions = rights descParseResult
    details = fromRight [] detailsParseResult
    skills = assembleSkills parsedDescriptions details
    traits = usedTraits skills $ assembleTraits parsedDescriptions

-- filter out traits that are not used by any skills
usedTraits :: [Skill] -> [Trait] -> [Trait]
usedTraits ss = filter (flip S.member usedTraitIds . traitId)
  where usedTraitIds = S.fromList $ concatMap (\s -> [primaryTrait s, secondaryTrait s]) ss
