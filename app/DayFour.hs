{-# LANGUAGE OverloadedStrings #-}

import Data.Set (fromList, size)
import Data.List (nub)
import qualified Data.Text as T
import Protolude

isValidPhrase1 :: [Text] -> Bool
isValidPhrase1 phrase = length phrase == size phraseSet
  where
    phraseSet = fromList phrase

isValidPhrase2 :: [Text] -> Bool
isValidPhrase2 phrase = length (sorted phrase) == length phrase
  where
    sorted ps = nub $ map sort $ map T.unpack ps

main :: IO ()
main = do
  fileContents <- readFile "input/day4.txt"
  let phrases = map (T.splitOn " ") (T.lines fileContents)
  do
    putStrLn("4-1 -----------"::Text)
    print (foldl (\acc cur -> if cur == True then (acc + 1)::Int else acc) 0
           $ map isValidPhrase1 phrases)
    putStrLn("4-2 -----------"::Text)
    print (foldl (\acc cur -> if cur == True then (acc + 1)::Int else acc) 0
           $ map isValidPhrase2 phrases)
