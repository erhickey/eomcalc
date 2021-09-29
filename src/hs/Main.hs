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
descriptionsFile = "/lua.unity3d/Assets/AssetBundles/lua.unity3d/temp/lua/config/language/en_us.lua.bytes"

-- relative path of skill details file
detailsFile :: String
detailsFile = "/lua.unity3d/Assets/AssetBundles/lua.unity3d/temp/lua/config/SkillConfig.lua.bytes"

-- data to write out to rarities.json
raritiesJson :: String
raritiesJson = "{\"rarities\":{\"COMMON\":1,\"UNCOMMON\":2,\"RARE\":3,\"EPIC\":4,\"LEGENDARY\":5}}"

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
  details <- readFileLatin1 $ rippedData ++ detailsFile
  putStrLn "Parsing data from game files..."
  case generateData descriptions details of
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
