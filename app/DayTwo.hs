import Data.List (concat, subsequences)
import Protolude
import Text.Parsec.Char
import qualified Text.Parsec as T
import Prelude (read, minimum, maximum)
import Text.Parsec.String

import Lib

rowMaxDifference :: [Int] -> Int
rowMaxDifference row = (maximum row) - (minimum row)

checksum :: [[Int]] -> Int
checksum rows = sum $ map rowMaxDifference rows

combinations :: Int -> [a] -> [[a]]
combinations k xs = filter ((k == ) . length) (subsequences xs)

rowEvenRemainders :: [Int] -> [(Int, Int)]
rowEvenRemainders row = [(a, b) |
                         [a, b] <- combinations 2 $ sorted row,
                         a `rem` b == 0]
  where
    sorted xs = reverse $ sort xs

checksum2 :: [[Int]] -> Int
checksum2 rows = sum
  $ map (\(a, b) -> a `div` b)
  $ concat
  $ map rowEvenRemainders rows

main :: IO ()
main = do
  parseInput "input/day2.txt" parser >>= \rows -> do
    putStrLn "2-1 ------"
    print (checksum rows)
    putStrLn "2-2 ------"
    print (checksum2 rows)
  where
    parser :: Parser [[Int]]
    parser = T.many1 (T.sepBy1 (read <$> T.many1 digit) (char '\t') <* newline)
