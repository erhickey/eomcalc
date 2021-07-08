{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as BS (readFile, writeFile)
import Data.Either (rights)
import Data.IntMap (IntMap)
import Data.IntMap as IM (empty, findWithDefault, fromList, toAscList)
import Data.Map (Map)
import Data.Map as M (empty, findWithDefault, insert)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (replace, Text)
import qualified Data.Text as T (lines, pack, unpack)
import GHC.Generics (Generic)
import System.IO (latin1, hSetEncoding, openFile, hGetContents, IOMode(ReadMode))

import Data.Attoparsec.Text (anyChar, char, decimal, double, many', manyTill, parseOnly, Parser, string, takeTill)
import Data.Aeson ((.:), (.:?), decode, encode, FromJSON, parseJSON, ToJSON, Value(Object))

newtype Skills = Skills { skills :: [Skill] } deriving (Eq, FromJSON, Generic, Show, ToJSON)

data Skill = Skill
  { skillName :: Text
  , skillId :: Int
  , skillType :: Int
  , rarity :: Int
  , primaryTrait :: Int
  , secondaryTrait :: Int
  , damage :: [Int]
  , mods :: [Int]
  , cooldowns :: [Double]
  , descriptions :: [String]
  } deriving (Eq, Generic, Show, ToJSON)

instance FromJSON Skill where
  parseJSON (Object v) =
    Skill
      <$> v .: "skillName"
      <*> v .: "skillId"
      <*> v .: "skillType"
      <*> v .: "rarity"
      <*> v .: "primaryTrait"
      <*> v .: "secondaryTrait"
      <*> v .: "damage"
      <*> v .: "mods"
      <*> (fromMaybe [] <$> v .:? "cooldowns")
      <*> (fromMaybe [] <$> v .:? "descriptions")
  parseJSON _ = mzero

data SkillDetail = SkillDetail
  { skillDetailName :: String
  , skillIds :: [Int]
  , skillDetailDescriptions :: [String]
  , skillDetailCooldowns :: [Double]
  }

data ParsedDataType = Name | Description | InvalidLine deriving (Eq)
type ParseDescriptionResult = (Int, String, ParsedDataType)

-- attempt to parse all lines of game data file for names and descriptions
descriptionLineParser :: Parser ParseDescriptionResult
descriptionLineParser = skillNameParser <|> skillDescriptionParser

-- attempt to parse line of game data file for skill name
skillNameParser :: Parser ParseDescriptionResult
skillNameParser = do
  _ <- string "\t[\"SkillName_"
  sId <- decimal
  _ <- string "\"] = \""
  skill <- manyTill anyChar (string "\",")
  pure (sId, skill, Name)

-- attempt to parse line of game data file for skill description
skillDescriptionParser :: Parser ParseDescriptionResult
skillDescriptionParser = do
  _ <- string "\t[\"SkillTips_"
  sId <- decimal
  _ <- string "\"] = \""
  description <- manyTill anyChar (string "\",")
  pure (sId, fixLuckyDice $ replaceSmartQuotes description, Description)

-- remove erroneous < from Lucky Dice descriptions
fixLuckyDice :: String -> String
fixLuckyDice = T.unpack . replace "<DEF" "DEF" . T.pack

-- replace multi-byte smart quote charactes with apostrophe
replaceSmartQuotes :: String -> String
replaceSmartQuotes = T.unpack . replace smartQuote "'" . T.pack
  where smartQuote = T.pack "\226\128\153"

-- attempt to parse game data file for skill cooldowns
skillCooldownsParser :: Parser [(Int, Double)]
skillCooldownsParser = catMaybes <$> many' (pure <$> skillCooldownParser <|> (detailsLine >> pure Nothing))

-- attempt to parse next id and cooldown in game data file
skillCooldownParser :: Parser (Int, Double)
skillCooldownParser = do
  _ <- string "\t\tID = "
  sId <- decimal
  _ <- char ','
  _ <- manyTill detailsLine (string "\t\tCD = ")
  cd <- double
  _ <- char ','
  pure (sId, cd)

-- match on a line from the details file (has no newline characters)
detailsLine :: Parser Text
detailsLine = takeTill ('\r' ==) <* char '\r'

-- get maps of skill id to names and descriptions from game data file
getSkillNamesAndDescriptions :: String -> IO (IntMap String, IntMap String)
getSkillNamesAndDescriptions file = do
  h <- openFile file ReadMode
  hSetEncoding h latin1
  contents <- T.lines . T.pack <$> hGetContents h
  let results = rights $ map (parseOnly descriptionLineParser) contents
      names = IM.fromList $ mapMaybe (getByType Name) results
      ds = IM.fromList $ mapMaybe (getByType Description) results
  pure (names, ds)
  where
    getByType dataType (sId, s, t)
      | t == dataType = pure (sId, s)
      | otherwise = Nothing

-- get map of skill id to cooldowns from game data file
getSkillCooldowns :: String -> IO (IntMap Double)
getSkillCooldowns file = do
  h <- openFile file ReadMode
  hSetEncoding h latin1
  contents <- T.pack . concat . drop 4 . init . lines <$> hGetContents h
  case parseOnly skillCooldownsParser contents of
    Left _ -> pure IM.empty
    Right result -> pure $ IM.fromList result

-- aggregate all data pulled from game data files into map of skill name to skill detail
buildSkillDetailMap
  :: IntMap String          -- map of id to skill name
  -> IntMap String          -- map of id to skill descriptions
  -> IntMap Double             -- map of id to cooldowns
  -> Map String SkillDetail -- map of skill name to aggregated skill details
buildSkillDetailMap sm dm cm = foldl go M.empty $ IM.toAscList sm
  where
    go acc (sId, sName) = M.insert sName (update existing) acc
      where
        existing = M.findWithDefault newSD sName acc
        newSD = SkillDetail sName [] [] []
        update (SkillDetail sdName sdIds sdDescs sdCds) =
          SkillDetail
            { skillDetailName = sdName
            , skillIds = sdIds ++ [sId]
            , skillDetailDescriptions = sdDescs ++ [IM.findWithDefault "" sId dm]
            , skillDetailCooldowns = sdCds ++ [IM.findWithDefault 0 sId cm]
            }

-- read skills.json
parseSkillsJSON :: String -> IO [Skill]
parseSkillsJSON s = skills . fromMaybe (Skills []) . decode <$> BS.readFile s

-- combine skills.json and data from game files
combineSkillsAndDetails :: [Skill] -> Map String SkillDetail -> [Skill]
combineSkillsAndDetails ss sm = foldl go [] ss
  where go acc s = skill:acc
          where sd = M.findWithDefault (SkillDetail "" [] [] []) (T.unpack $ skillName s) sm
                ds = skillDetailDescriptions sd
                cs = skillDetailCooldowns sd
                skill = Skill
                  { skillName = skillName s
                  , skillId = skillId s
                  , skillType = skillType s
                  , rarity = rarity s
                  , primaryTrait = primaryTrait s
                  , secondaryTrait = secondaryTrait s
                  , damage = damage s
                  , mods = mods s
                  , cooldowns = cs
                  , descriptions = ds
                  }

-- write generated skills to file
writeSkills :: String -> [Skill] -> IO ()
writeSkills file = BS.writeFile file . encode . Skills

main :: IO ()
main = do
  ss <- parseSkillsJSON "src/data/skills.json"
  (nm, dm) <- getSkillNamesAndDescriptions "src/data/skill-descriptions"
  cm <- getSkillCooldowns "src/data/skill-details"
  let sdm = buildSkillDetailMap nm dm cm
      output = combineSkillsAndDetails ss sdm
  writeSkills "src/data/skills-generated.json" output
