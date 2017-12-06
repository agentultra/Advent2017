{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (makeLenses)
import Prelude (read)
import Protolude
import qualified Text.Parsec as T
import Text.Parsec.Char
import Text.Parsec.String

import Lib

data MemoryState = MemoryState
  { _current :: Int
  , _banks :: [Int]
  , _seen :: [[Int]]
  }

makeLenses ''MemoryState

runProgram :: [Int] -> Int
runProgram mem = evalState (myProg 0) (MemoryState { _current=0, _banks=mem })
  where
    myProg :: Int -> State MemoryState Int
    myProg step = do
      c <- use current
      bs <- use banks
      s <- use seen



main :: IO ()
main = do
  parseInput "input/day6.txt" parser >>= \buckets -> do
    print buckets
  where
    parser :: Parser [Int]
    parser = T.sepBy1 (read <$> T.many1 digit) (char '\t') <* newline
