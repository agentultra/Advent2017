{-# LANGUAGE TemplateHaskell #-}

import Control.Lens ((+=), (^?), ix, makeLenses, use)
import Prelude (read)
import Protolude hiding ((&))
import qualified Text.Parsec as P
import Text.Parsec.String

import Lib

data ProgramState = ProgramState
  { _counter :: Int
  , _program :: [Int]
  }

makeLenses ''ProgramState

runProgram :: [Int] -> Int
runProgram prog = evalState (myProg 0) (ProgramState { _counter=0, _program=prog })
  where
    myProg :: Int -> State ProgramState Int
    myProg step = do
      c <- use counter
      p <- use program
      case p ^? ix c of
        Just x -> do
          counter += x
          program . ix c += 1
          myProg (step+1)
        Nothing -> return step

runProgram2 :: [Int] -> Int
runProgram2 prog = evalState (myProg 0) (ProgramState { _counter=0, _program=prog })
  where
    myProg :: Int -> State ProgramState Int
    myProg step = do
      c <- use counter
      p <- use program
      case p ^? ix c of
        Just x -> do
          counter += x
          program . ix c += if c - (abs x) >= 3 then -1 else 1
          myProg (step+1)
        Nothing -> return step

main :: IO ()
main = do
  parseInput "input/day5.txt" parser >>= \nums -> do
    putStrLn "5-1 -------------"
    print (runProgram nums)
    putStrLn "5-2 -------------"
    print (runProgram2 nums)

  where
    parser :: Parser [Int]
    parser = P.many ((read <$> P.many1 (P.char '-' <|> P.digit)) <* P.newline)
