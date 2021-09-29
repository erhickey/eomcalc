{-# LANGUAGE OverloadedStrings #-}

module EomJson.Assembler.Traits (assembleTraits) where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import qualified Data.Text as T (splitOn)

import EomJson.Json.Trait (Trait(..))
import EomJson.Parse.SkillsAndTraits (FieldType(..), ParsedField)

-- assemble traits from data parsed from game files
assembleTraits :: [ParsedField] -> [Trait]
assembleTraits = map assembleTrait . groupParsedTraitFields

-- groups of parsed fields that represent traits
groupParsedTraitFields :: [ParsedField] -> [[ParsedField]]
groupParsedTraitFields = groupBy ((==) `on` fst') . sortOn fst' . filter isParsedTraitField
  where fst' (x, _, _) = x

-- if a parsed field holds trait data
isParsedTraitField :: ParsedField -> Bool
isParsedTraitField (_, _, ft) = ft == TraitName || ft == TraitDescription || ft == TraitMods

-- assemble trait from parsed fields, assumes the parsed fields all belong to the same trait
assembleTrait :: [ParsedField] -> Trait
assembleTrait = foldl go (Trait "" 0 "" [])
  where
    go (Trait _ _ desc ms) (tId, f, TraitName) = Trait f tId desc ms
    go (Trait name _ _ ms) (tId, f, TraitDescription) = Trait name tId f ms
    go (Trait name _ desc _) (tId, f, TraitMods) = Trait name tId desc (T.splitOn "|" f)
    go acc _ = acc
