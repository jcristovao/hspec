module Main (main, spec) where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec


action :: IO String
action = do
  putStrLn "This is only run once!"
  readFile "example/Spec.hs"

spec :: Spec
spec = beforeAll action $ do
  describe "example/Spec.hs" $ do
    it "gives the original list, if applied twice" $ \input -> do
      input `shouldContain` "foo"

    it "gives the original list, if applied twice" $ \input -> do
      input `shouldContain` "bar"

    it "gives the original list, if applied twice" $ \input ->
      property $ \xs -> do
        -- also works for QC properties, also it's not awfully useful for this
        -- particular example...
        (reverse . reverse) xs `shouldBe` (xs :: [Int])
