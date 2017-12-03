{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Protolude

{-

17 16 15 14 13
18  5  4  3 12
19  6  1  2 11
20  7  8  9 10
21 22 23 24 25

We want to find the manhattan distance to the origin given a number,
`n`.

Notice that the bottom-right corners of each concentric square is a
perferct odd square: 9, 25, 49, 81.

This means we can calculate the nearest odd-perfect-square from n^2
and calculate the offset from the side of the "ring" we're on.

I suspect there's also a matrix to transform `n` to a co-ordinate on
the plane which would allow us to simplify a little more... but this
works too. :)

-}


distance :: Int -> Int
distance n = ringN + abs (offset - ringN)
  where
    root = ceiling $ sqrt (fromIntegral n)
    ring = if root `rem` 2 /= 0 then root else root + 1
    ringN = (ring - 1) `div` 2
    cycleR = n - ((ring - 2) ^ 2)
    offset = cycleR `rem` (ring - 1)

main :: IO ()
main = do
  putStrLn "3-1 ------"
  print (distance 312051)
