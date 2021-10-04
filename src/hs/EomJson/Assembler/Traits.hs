{-# LANGUAGE OverloadedStrings #-}

module EomJson.Assembler.Traits (assembleTraits) where

import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM (findWithDefault, fromList)
import Data.List (groupBy, sortOn)
import qualified Data.Text as T (splitOn)

import EomJson.Json.Trait (Trait(..))
import EomJson.Parse.SkillsAndTraits (FieldType(..), ParsedField)
import EomJson.Parse.TraitDetails (TraitDetail(..))

-- assemble traits from data parsed from game files
assembleTraits :: [ParsedField] -> [TraitDetail] -> [Trait]
assembleTraits ps ts = map (addDetails tm . assembleTrait) $ groupParsedTraitFields ps
  where tm = IM.fromList $ map (\td -> (traitDetailId td, td)) ts

-- groups of parsed fields that represent traits
groupParsedTraitFields :: [ParsedField] -> [[ParsedField]]
groupParsedTraitFields = groupBy ((==) `on` fst') . sortOn fst' . filter isParsedTraitField
  where fst' (x, _, _) = x

-- if a parsed field holds trait data
isParsedTraitField :: ParsedField -> Bool
isParsedTraitField (_, _, ft) = ft == TraitName || ft == TraitDescription || ft == TraitMods

-- assemble trait from parsed fields, assumes the parsed fields all belong to the same trait
assembleTrait :: [ParsedField] -> Trait
assembleTrait = foldl go (Trait "" 0 "" [] [])
  where
    go (Trait _ _ desc _ ms) (tId, f, TraitName) = Trait f tId desc [] ms
    go (Trait name _ _ _ ms) (tId, f, TraitDescription) = Trait name tId f [] ms
    go (Trait name _ desc _ _) (tId, f, TraitMods) = Trait name tId desc [] (T.splitOn "|" f)
    go acc _ = acc

addDetails :: IntMap TraitDetail -> Trait -> Trait
addDetails tm (Trait name tId desc _ ms) = Trait name tId desc bs ms
  where bs = traitDetailBreakpoints $ IM.findWithDefault (TraitDetail 0 []) tId tm
