module Main (main) where

import qualified Data.ByteString.Lazy as BS (writeFile)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.Environment (getArgs)
import System.IO (latin1, hSetEncoding, openFile, hGetContents, IOMode(ReadMode))

import Data.Aeson (encode)

import EomJson.EomJson (generateData)
import EomJson.Json.Skill (Skill, Skills(..))
import EomJson.Json.Trait (Trait, Traits(..))

-- write generated skills to file
writeSkills :: String -> [Skill] -> IO ()
writeSkills file = BS.writeFile file . encode . Skills

-- write generated traits to file
writeTraits :: String -> [Trait] -> IO ()
writeTraits file = BS.writeFile file . encode . Traits

-- read file contents
readFileLatin1 :: String -> IO Text
readFileLatin1 file = do
  h <- openFile file ReadMode
  hSetEncoding h latin1
  T.pack <$> hGetContents h

-- takes two arguments, location of the descriptions file, then the location of the details file
main :: IO ()
main = do
  [descriptionsFile, detailsFile] <- getArgs
  descIn <- readFileLatin1 descriptionsFile
  detailsIn <- readFileLatin1 detailsFile
  putStrLn "Parsing data from game files..."
  case generateData descIn detailsIn of
    (Left xs) -> do
      putStrLn "Parsing failed, errors:"
      mapM_ putStrLn xs
    (Right (ss, ts)) -> do
      putStrLn "Parsing succeeded!"
      putStrLn $ "Found " ++ show (length ss) ++ " skills and " ++ show (length ts) ++ " traits."
      putStrLn "Writing skills to src/data/skills.json"
      writeSkills "src/data/skills.json" ss
      putStrLn "Writing traits to src/data/traits.json"
      writeTraits "src/data/traits.json" ts
      putStrLn "Writing rarities to src/data/rarities.json"
      writeFile "src/data/rarities.json" raritiesJson

raritiesJson :: String
raritiesJson = "{\"rarities\":{\"COMMON\":1,\"UNCOMMON\":2,\"RARE\":3,\"EPIC\":4,\"LEGENDARY\":5}}"
