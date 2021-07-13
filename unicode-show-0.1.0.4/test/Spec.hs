{-# LANGUAGE ScopedTypeVariables #-}


import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Text.Show.Unicode

data T試6験 = Å4 { すけろく :: String} deriving (Eq, Ord, Show, Read)
data T試7験 = String :@\& String  deriving (Eq, Ord, Show, Read)
data T試8験 = String :＠\& String  deriving (Eq, Ord, Show, Read)


ushowTo :: Show a => a -> String -> Spec
ushowTo f t = it ("ushow " ++ show f ++ " == " ++ t) $ t `shouldBe` ushow f

spec :: Spec
spec =
  describe "individual representations test" $ do
    describe "individual representations test" $ do
      "صباح الخير" `ushowTo` "\"صباح الخير\""
      "😆💕>λ\\=🐘" `ushowTo`  "\"😆💕>λ\\\\=🐘\""
      "漢6" `ushowTo` "\"漢6\""
      "\32\&7" `ushowTo` "\" 7\""
      "改\n行" `ushowTo` "\"改\\n行\""
      "下一站\na\ri\ta国际机场" `ushowTo` "\"下一站\\na\\ri\\ta国际机场\""
      "\SOH\SO\&H" `ushowTo` "\"\\SOH\\SO\\&H\""

    describe "read . ushow == id" $ do
      prop "read . ushow == id, for String" $
        \str -> read (ushow str) `shouldBe` (str :: String)

      prop "read . ushow == id, for Char" $
        \x -> read (ushow x) `shouldBe` (x :: Char)

      prop "read . ushow == id, for [(Char,())]" $
        \x -> read (ushow x) `shouldBe` (x :: [(Char,())])

      prop "read . read . ushow . ushow == id, for String" $
        \str -> (read $ read $ ushow $ ushow str) `shouldBe` (str :: String)

      prop "read . ushow == id, for some crazy Unicode type" $
        \str -> let v = Å4 str in read (ushow v) `shouldBe` v

      prop "read . ushow == id, for some crazy Unicode type" $
        \a b -> let v = a :@\& b in read (ushow v) `shouldBe` v

      prop "read . ushow == id, for some crazy Unicode type" $
        \a b -> let v = a :＠\& b in read (ushow v) `shouldBe` v

      prop "read . ushow == id, for compound type" $
        \str -> read (ushow str) `shouldBe` (str :: Either [String] (String,String))

main :: IO ()
main = do
  print $ "hoge" :@\& "huga"
  putStrLn $ ushow $ "hoge" :@\& "huga"
  hspec spec
