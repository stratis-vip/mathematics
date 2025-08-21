module Main where

import Data.Algebra.Helpers (clearScreen)
import System.Exit (exitSuccess)
import XrcsA1
import XrcsA2
import XrcsA3
import XrcsA4

-- import System.Process (callCommand)
-- import Test.Hspec

main :: IO ()
main = do
  clearScreen
  putStrLn
    "ΚΕΦΑΛΑΙΟ 1\n\
    \Πιθανότητες - Δειγματικός χώρος\n\n\
    \ΑΣΚΗΣΕΙΣ Α' ΟΜΑΔΑΣ\n\
    \==================\n\
    \1. Άσκηση 1\n\
    \2. Άσκηση 2\n\
    \3. Άσκηση 3\n\
    \4. Άσκηση 4\n\
    \\nΔώστε επιλογή (0 για έξοδο)."
  option <- getLine
  case read option :: Int of
    1 -> execute exercise_1
    2 -> execute exercise_2
    3 -> execute exercise_3
    4 -> execute exercise_4
    _ -> exitSuccess

execute :: IO () -> IO ()
execute f = do
  f
  putStrLn "\n\nΠίεσε οτιδήποτε για επιστροφή στις επιλογές…"
  _ <- getLine
  main
