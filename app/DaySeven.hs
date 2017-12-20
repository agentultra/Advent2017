{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
import Data.Maybe (fromJust)
import Prelude (read)
import Protolude
import qualified Text.Parsec as T
import Text.Parsec.Char
import Text.Parsec.String

import Lib
import Seven

main :: IO ()
main = do
  parseInput "input/day7.txt" parser >>= \programs -> do
    putStrLn ("7-1 ------------"::Text)
    case findBottom programs of
      Just bottom -> print bottom
      Nothing -> putStrLn ("Could not find the bottom"::Text)
    putStrLn ("7-2 ------------"::Text)
    print (fromJust $ findUnbalanced $ buildTree $ buildProgramMap programs)
  where
    nameParser :: Parser Name
    nameParser = T.many1 lower
    weightParser :: Parser Int
    weightParser = (read <$> (T.between (char '(') (char ')') (T.many1 digit)))
    nameListParser :: Parser [Name]
    nameListParser = nameParser `T.sepBy` (char ',' <* (T.many space))
    discParser :: Parser [Name]
    discParser = (T.skipMany (string " ->" <* space)) *> nameListParser
    parser :: Parser [Program]
    parser = T.many ((liftM3 Program (nameParser <* space) weightParser (optional (discParser))) <* newline)
