{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module EomJson.Json.Skill (Skill(..), Skills(..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

newtype Skills = Skills { skills :: [Skill] } deriving (Generic, ToJSON)

data Skill = Skill
  { skillName :: Text
  , skillId :: Int
  , rarity :: Int
  , isActive :: Bool
  , primaryTrait :: Int
  , secondaryTrait :: Int
  , cooldowns :: [Double]
  , descriptions :: [Text]
  } deriving (Generic, ToJSON)

instance Semigroup Skill where
  (Skill name sId rar active pTrait sTrait cs1 ds1) <> (Skill _ _ _ _ _ _ cs2 ds2) =
    Skill name sId rar active pTrait sTrait (cs1 ++ cs2) (ds1 ++ ds2)
