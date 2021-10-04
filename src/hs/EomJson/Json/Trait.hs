{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module EomJson.Json.Trait (Trait(..), Traits(..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (ToJSON)

newtype Traits = Traits { traits :: [Trait] } deriving (Generic, ToJSON)

data Trait = Trait
  { traitName :: Text
  , traitId :: Int
  , traitDescription :: Text
  , traitBreakpoints :: [Int]
  , traitMods :: [Text]
  } deriving (Generic, ToJSON)
