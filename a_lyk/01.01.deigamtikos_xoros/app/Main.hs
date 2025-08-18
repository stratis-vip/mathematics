module Main where

import System.Exit (exitSuccess)
import Test.Hspec

main :: IO ()
main = do
  putStrLn
    "ΑΣΚΗΣΕΙΣ Α' ΟΜΑΔΑΣ\n\
    \==================\n\n\
    \1. Άσκηση 1\n\n\
    \Δώστε επιλογή."
  option <- getLine
  case read option :: Int of
    1 -> exercise_1
    0 -> exitSuccess
    _ -> exitSuccess

exercise_1 :: IO ()
exercise_1 = do
  putStrLn
    "1. Ένα κουτί έχει τρεις μπάλες, μια άσπρη, μια μαύρη και μια κόκκινη. Κάνουμε το\n\
    \εξής πείραμα: παίρνουμε από το κουτί μια μπάλα, καταγράφουμε το χρώμα της\n\
    \και την ξαναβάζουμε στο κουτί. Στη συνέχεια παίρνουμε μια δεύτερη μπάλα και\n\
    \καταγράφουμε επίσης το χρώμα της. (Όπως λέμε παίρνουμε διαδοχικά δύο μπάλες\n\
    \με επανατοποθέτηση).\n\
    \    i) Ποιος είναι ο δειγματικός χώρος του πειράματος;\n\
    \   ii) Ποιο είναι το ενδεχόμενο \"η πρώτη μπάλα να είναι κόκκινη\";\n\
    \  iii) Ποιο είναι το ενδεχόμενο \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα;\n\n\
    \ΛΥΣΗ\n\
    \----\n\
    \Οι πιθανές τιμές είναι\n\
    \'A' -> Άσπρη μπάλα\n\
    \'M' -> Μαύρη μπάλα\n\
    \'K' -> Κόκκινη μπάλα\n"

  putStr
    "  i) Εφόσον υπάρχει επανατοποθέτηση τότε ο δειγματικός χώρος Ω είναι το καρτεσιανό \n\
    \γινόμενο \"ΑΚM\" x \"AKM\" Ω = "

  -- οι πιθανές τιμές είναι Α, Μ , Κ
  let w = "AMK" :: String
      _W = cartesianProduct w w

  print _W

  -- ii η πρώτη μπάλα να είναι κοκκινη
  let _A = filter (\(a, _) -> a == 'K') _W
  putStr
    "\n ii) Για το ενδεχόμενο Α ⊆ Ω, ισχύει Α = {(x,y) ∈ Ω | x = 'Κ'}. Οπότε\n\
    \  A = "
  print _A
  putStr
    "\niii) Για το ενδεχόμενο B ⊆ Ω, ισχύει B = {(x,y) ∈ Ω | x = y }. Οπότε\n\
    \  B = "
  print (filter (\(a, b) -> a == b) _W)
  main

-- | Υπολογίζει το καρτεσιανό γινόμενο μεταξύ δυο συνόλων.
cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct a b = [(x, y) | x <- a, y <- b]

runTests :: IO ()
runTests = hspec $ do
  describe
    "Oι δειγματικοί χώροι είναι το καρτεσιανό γινόμενο των συνόλων των\n\
    \αποτελεσμάτων!"
    $ do
      it "∅ × B = ∅" $ do
        cartesianProduct ([] :: [Int]) [1, 2] `shouldBe` []
      it "A × ∅ = ∅" $ do
        cartesianProduct [1, 2 :: Int] [] `shouldBe` []
      it "A × B = [(1,3),(1,4),(2,3),(2,4)]" $ do
        cartesianProduct [1, 2] [3, 4 :: Int] `shouldBe` [(1, 3), (1, 4), (2, 3), (2, 4)]

      it "το πλήθος του δειγματικού χώρου, είναι το γινόμενο του πλήθους των δυο συνόλων Α, Β" $ do
        length (cartesianProduct [1 .. 4] [5 .. 8 :: Int]) `shouldBe` 16
        length (cartesianProduct "abcde" "gfhijkl") `shouldBe` 35
