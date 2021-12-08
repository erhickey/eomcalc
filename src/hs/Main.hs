module Main (main) where

import qualified Data.ByteString.Lazy as BS (writeFile)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.Environment (getArgs)
import System.IO (latin1, hSetEncoding, openFile, hGetContents, IOMode(ReadMode))

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)

import EomJson.EomJson (generateData)
import EomJson.Json.Skill (Skills(..))
import EomJson.Json.Trait (Traits(..))

-- relative path of english skill and traits description file
descriptionsFile :: String
descriptionsFile = "/Assets/Asset_Bundles/lua.unity3d/temp/lua/config/language/en_us.lua.txt"

-- relative path of skill details file
skillDetailsFile :: String
skillDetailsFile = "/Assets/Asset_Bundles/lua.unity3d/temp/lua/config/SkillConfig.lua.txt"

-- relative path of trait detailsfile
traitDetailsFile :: String
traitDetailsFile = "/Assets/Asset_Bundles/lua.unity3d/temp/lua/config/SkillTraitConfig.lua.txt"

-- data to write out to rarities.json
raritiesJson :: String
raritiesJson = "{\"rarities\":[{\"key\":1,\"value\":\"COMMON\"},{\"key\":2,\"value\":\"UNCOMMON\"},{\"key\":3,\"value\":\"RARE\"},{\"key\":4,\"value\":\"EPIC\"},{\"key\":5,\"value\":\"LEGENDARY\"}]}"

-- encode/write json string in human readable format
writeJsonFile :: ToJSON a => String -> a -> IO ()
writeJsonFile file = BS.writeFile file . encodePretty

-- read file contents
readFileLatin1 :: String -> IO Text
readFileLatin1 file = do
  h <- openFile file ReadMode
  hSetEncoding h latin1
  T.pack <$> hGetContents h

-- takes one argument, location of the ripped lua files
main :: IO ()
main = do
  [rippedData] <- getArgs
  descriptions <- readFileLatin1 $ rippedData ++ descriptionsFile
  skillDetails <- readFileLatin1 $ rippedData ++ skillDetailsFile
  traitDetails <- readFileLatin1 $ rippedData ++ traitDetailsFile
  putStrLn "Parsing data from game files..."
  case generateData descriptions skillDetails traitDetails of
    (Left xs) -> do
      putStrLn "Parsing failed, errors:"
      mapM_ putStrLn xs
    (Right (ss, ts)) -> do
      putStrLn "Parsing succeeded!"
      putStrLn $ "Found " ++ show (length ss) ++ " skills and " ++ show (length ts) ++ " traits."
      putStrLn "Writing skills to src/data/skills.json"
      writeJsonFile "src/data/skills.json" $ Skills ss
      putStrLn "Writing traits to src/data/traits.json"
      writeJsonFile "src/data/traits.json" $ Traits ts
      putStrLn "Writing rarities to src/data/rarities.json"
      writeFile "src/data/rarities.json" raritiesJson
