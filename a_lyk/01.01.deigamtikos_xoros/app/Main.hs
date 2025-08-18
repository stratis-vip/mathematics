module Main where

import Test.Hspec

main :: IO ()
main = do
  putStrLn
    "ΑΣΚΗΣΕΙΣ Α' ΟΜΑΔΑΣ\n\n\
    \1. Ένα κουτί έχει τρεις μπάλες, μια άσπρη, μια μαύρη και μια κόκκινη. Κάνουμε το\n\
    \εξής πείραμα: παίρνουμε από το κουτί μια μπάλα, καταγράφουμε το χρώμα της\n\
    \και την ξαναβάζουμε στο κουτί. Στη συνέχεια παίρνουμε μια δεύτερη μπάλα και\n\
    \καταγράφουμε επίσης το χρώμα της. (Όπως λέμε παίρνουμε διαδοχικά δύο μπάλες\n\
    \με επανατοποθέτηση).\n\
    \    i) Ποιος είναι ο δειγματικός χώρος του πειράματος;\n\
    \   ii) Ποιο είναι το ενδεχόμενο \"η πρώτη μπάλα να είναι κόκκινη\";\n\
    \  iii) Ποιο είναι το ενδεχόμενο \"να εξαχθεί και τις δυο φορές μπάλα με το ίδιο χρώμα;\n\n\
    \ΛΥΣΗ\n\
    \Οι πιθανές τιμές είναι\n\
    \'A' -> Άσπρη μπάλα\n\
    \'M' -> Μαύρη μπάλα\n\
    \'K' -> Κόκκινη μπάλα\n"

  putStr "  i) Ω = "
  -- οι πιθανές τιμές είναι Α, Μ , Κ
  let w = "AMK" :: String
      wΩ = cartesianProduct w w

  print wΩ

  -- ii η πρώτη μπάλα να είναι κοκκινη
  let _A = filter (\(a, _) -> a == 'K') wΩ
  putStr " ii) A = "
  print _A
  putStr "iii) B = "
  print (filter (\(a, b) -> a == b) wΩ)

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
