module XrcsB1 (exercise_b1) where

import Data.Algebra.Helpers
import Data.List (nub)

exercise_b1 :: IO ()
exercise_b1 = do
  -- \| ΕΚΦΩΝΗΣΗ
  prepareText $
    "1. Δύο παίκτες θα παίξουν σκάκι και συμφωνούν νικητής να είναι εκείνος που πρώτος θα κερδίσει δύο παιχνίδια. Αν α είναι το αποτέλεσμα να κερδίσει ο πρώτος παίκτης ένα παιχνίδι και β είναι το αποτέλεσμα να κερδίσει ο δεύτερος παίκτης ένα παιχνίδι, να γράψετε το δειγματικό χώρο του πειράματος."

  -- \| ΔΕΔΟΜΕΝΑ

  let _W1 = ['α', 'β'] :: [Char]
  let _W = [(x, y, z) | x <- _W1, y <- _W1, z <- _W1]

  --
  -- \| ΛΥΣΗ
  prepareText $
    "\nΛΥΣΗ\n----\n\
    \ O δειγματικός χώρος είναι ω1 = { α, β } για τη πρώτη παρτίδα, ω2 = ω1 για τη 2η και ω3 = ω1 για τη 3η. Οπότε Ω = { (x, y) ∈ { α, β }² | x = y } ∪ { (x, y, z) ∈ { α, β }³ | x ≠ y } : \n "
      ++ formatSampleSpace stringFormatter "Ω" (nub . map checkSolution $ _W)

-- | ελέγχει πότε έχει τελειωσει η παρτίδα. Αν κάποιος συπμλήρωσε 2 νίκες δηλαδή
checkSolution :: (Char, Char, Char) -> String
checkSolution (x, y, z)
  | x == y = [x, x]
  | x == z = [x, y, z]
  | y == z = [x, y, z]
  | otherwise = []

stringFormatter :: String -> String
stringFormatter xs = xs
