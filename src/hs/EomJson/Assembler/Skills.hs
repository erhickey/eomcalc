{-# LANGUAGE OverloadedStrings #-}

module EomJson.Assembler.Skills (assembleSkills) where

import Data.Function (on)
import Data.IntMap ((!), IntMap)
import Data.IntMap as IM (alter, empty, member)
import Data.List (groupBy, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import EomJson.Json.Skill (Skill(..))
import EomJson.Parse.SkillsAndTraits (FieldType(..), ParsedField)
import EomJson.Parse.SkillDetails (SkillDetail(..))

maxSkillLevel :: Int
maxSkillLevel = 4

-- Skill Id, Skill Name, Skill Description
type SkillTexts = (Int, Text, Text)

-- assemble skills from skill description and detail data parsed from game files
assembleSkills :: [ParsedField] -> [SkillDetail] -> [Skill]
assembleSkills ps ss = map mergeSkills playerSkills
  where
    -- partial skills grouped by name
    partialSkills = groupBy ((==) `on` skillName) . sortOn skillName $ assemblePartialSkills ps ss
    -- filter out any groups that don't have at least maxSkillLevel elements
    playerSkills = filter ((==) maxSkillLevel . length) partialSkills

-- create partial skills from skill descriptions and skill details
-- partial skills contain data for one level of the skill
assemblePartialSkills :: [ParsedField] -> [SkillDetail] -> [Skill]
assemblePartialSkills ps = mapMaybe (assemblePartialSkill sm)
  where sm = assembleSkillDescriptions ps

-- IntMap of skill id to SkillDescription assembled from ParsedFields
assembleSkillDescriptions :: [ParsedField] -> IntMap SkillTexts
assembleSkillDescriptions = foldl go IM.empty
  where
    go acc sf@(_, _, SkillName) = update sf acc
    go acc sf@(_, _, SkillDescription) = update sf acc
    go acc _ = acc
    update sf@(sId, _, _) = IM.alter (combineParsedSkillFields sf) sId

-- combine SkillField and SkillDescription
-- if SkillDescription is Nothing, the other field in the result will be a blank string
combineParsedSkillFields :: ParsedField -> Maybe SkillTexts -> Maybe SkillTexts
combineParsedSkillFields (sId, f, SkillName) (Just (_, _, d)) = pure (sId, f, d)
combineParsedSkillFields (sId, f, SkillName) Nothing = pure (sId, f, "")
combineParsedSkillFields (sId, f, SkillDescription) (Just (_, n, _)) = pure (sId, n, f)
combineParsedSkillFields (sId, f, SkillDescription) Nothing = pure (sId, "", f)
combineParsedSkillFields _ _ = Nothing

-- attempt to find SkillDescription with id matching SkillDetail's
-- if found, combine the two and create a partial skill
assemblePartialSkill :: IntMap SkillTexts -> SkillDetail -> Maybe Skill
assemblePartialSkill m sd
  | IM.member (detailId sd) m = pure $ combineDescriptionAndDetail (m ! detailId sd) sd
  | otherwise = Nothing

-- combine skill SkillDetail and SkillDescription to create Skill
-- function assumes skill ids match in SkillDetail and SkillDescription
combineDescriptionAndDetail :: SkillTexts -> SkillDetail -> Skill
combineDescriptionAndDetail (sId, name, description) (SkillDetail _ qual _ active cd pTrait sTrait) =
  Skill name sId qual active pTrait sTrait [cd] [description]

-- merge partial skills, assume they are the same skill
mergeSkills :: [Skill] -> Skill
mergeSkills = foldl1 (<>) . sortOn skillId
