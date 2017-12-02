module Lib
  ( parseInput
  ) where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import Protolude

parseInput :: FilePath -> Parser a -> IO a
parseInput path parser = do
  result <- parseFromFile (parser <* eof) path
  either (panic . show) return result
