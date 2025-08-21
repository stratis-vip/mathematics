module XrcsB2 (exercise_b2) where

import Data.Algebra.Helpers

exercise_b2 :: IO ()
exercise_b2 = do
  -- \| ΕΚΦΩΝΗΣΗ
  prepareText $
    "2. Ρίχνουμε ένα ζάρι δύο φορές. Να βρείτε τα ενδεχόμενα:\n\
    \   Α: \"Το αποτέλεσμα της 1ης ρίψης είναι μεγαλύτερο από το αποτέλεσμα της 2ης ρίψης\".\n\
    \   Β: \"Το άθροισμα των ενδείξεων στις δύο ρίψεις είναι άρτιος αριθμός\".\n\
    \   Γ: \"Το γινόμενο των ενδείξεων στις δύο ρίψεις είναι μικρότερο του 5\".\n\
    \Στη συνέχεια να βρείτε τα ενδεχόμενα Α ∩ Β, Α ∩ Γ, Β ∩ Γ, (Α ∩ Β) ∩ Γ. "

  -- \| ΔΕΔΟΜΕΝΑ

  let _W1 = [1 .. 6] :: [Int]
  let _W = [(x, y) | x <- _W1, y <- _W1]
  let _A = [(x, y) | x <- [1 .. 6], y <- [1 .. 6], y < x]
  let _B = filter (\(x, y) -> (x + y) `mod` 2 == 0) _W
  let _C = filter (\(x, y) -> (x * y) < 5) _W
  let aTb = [x | x <- _A, elem x _B]
  let aTc = [x | x <- _A, elem x _C]

  let bTc = [x | x <- _B, elem x _C]

  let aTbTc = [x | x <- aTb, elem x _C]
  --
  -- \| ΛΥΣΗ
  prepareText $
    "\nΛΥΣΗ\n----\n\
    \O δειγματικός χώρος είναι Ω =  { (x, y) | x, y ∈ { 1, 2, 3, 4, 5, 6 }  } : \n    "
      ++ formatSampleSpace intTupleFormatter "Ω" _W
      ++ "\n\nA = { (x, y) x,y ∈ { 1, 2, 3, 4, 5, 6 } | x < y }\n     "
      ++ formatSampleSpace intTupleFormatter "Α" _A
      ++ "\n\nB = { (x, y) x,y ∈ { 1, 2, 3, 4, 5, 6 } | (x + y) άρτιος αριθμός } : \n    "
      ++ formatSampleSpace intTupleFormatter "B" _B
      ++ "\n\nΓ = { (x, y) x,y ∈ { 1, 2, 3, 4, 5, 6 } | (x ∙ y) < 5} : \n    "
      ++ formatSampleSpace intTupleFormatter "Γ" _C
      ++ "\n\n"
      ++ formatSampleSpace intTupleFormatter "A ∩ B" aTb
      ++ "\n\n"
      ++ formatSampleSpace intTupleFormatter "A ∩ Γ" aTc
      ++ "\n\n"
      ++ formatSampleSpace intTupleFormatter "B ∩ Γ" bTc
      ++ "\n\n"
      ++ formatSampleSpace intTupleFormatter "(Α ∩ Β) ∩ Γ" aTbTc

intTupleFormatter :: (Int, Int) -> String
intTupleFormatter (a, b) = "( " ++ show a ++ ", " ++ show b ++ " )"
