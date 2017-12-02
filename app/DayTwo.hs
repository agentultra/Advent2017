import Protolude
import qualified Text.Parsec as T
import Text.Parsec.Char
import Text.Parsec.String
import Prelude (read, minimum, maximum)

import Lib

rowDifference :: [Int] -> Int
rowDifference row = (maximum row) - (minimum row)

checksum :: [[Int]] -> Int
checksum rows = sum $ map rowDifference rows

main :: IO ()
main = do
  parseInput "input/day2.txt" parser >>= \rows -> do
    putStrLn "2-1 ------"
    print (checksum rows)
  where
    parser :: Parser [[Int]]
    parser = T.many1 (T.sepBy1 (read <$> T.many1 digit) (char '\t') <* newline)
