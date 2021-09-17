{-# LANGUAGE OverloadedStrings #-}

module EomJson.Util.Util (compose, replaceSmartQuotes, smartQuote) where

import Data.Text (Text)
import qualified Data.Text as T (pack, replace)

-- compose list of functions
-- functions will be applied in reverse order
-- flip the composition combinator to maintain order
compose :: [ a -> a ] -> (a -> a)
compose = foldl (.) id

-- Text representing multi-byte smart quote characters
smartQuote :: Text
smartQuote = T.pack "\226\128\153"

-- replace multi-byte smart quote characters with apostrophe
replaceSmartQuotes :: Text -> Text
replaceSmartQuotes = T.replace smartQuote "'"
