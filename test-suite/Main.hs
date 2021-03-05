import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import Data.List
import Data.Maybe
import qualified Data.Map as M

import Lib

type Vigenere = [Char] -> [Int] -> String -> String

data MAK = MAK { alphabet:: String, message :: String, key :: [Int]}
  deriving Show

instance Arbitrary MAK where
  arbitrary = do
    -- let a = ascii
    a <- (nub <$> listOf1 arbitraryPrintableChar) `suchThat` ((>=2) . length)
    let alen = length a
    MAK a
      <$> listOf (elements a)
      <*> listOf1 (chooseInt (1, alen-1))
  shrink MAK{} = []

bothWays :: (Vigenere -> Vigenere -> Bool) -> Bool
bothWays f = f vigenere unvigenere && f unvigenere vigenere

prop_encodes
  , prop_inverse
  , prop_zerokey
  , prop_emptymsg
  , prop_negKey
  , prop_differences
  :: MAK -> Bool

prop_shift :: MAK -> Positive Int -> Bool

prop_encodes (MAK a m k) = bothWays $ \enc _ -> and $ zipWith (/=) (enc a k m) m
prop_inverse (MAK a m k) = bothWays $ \enc dec -> dec a k (enc a k m) == m
prop_zerokey (MAK a m _) = bothWays $ \enc _ -> let k = [0] in enc a k m == m
prop_emptymsg (MAK a _ k) = bothWays $ \enc _ -> enc a k "" == ""
prop_shift (MAK a m k) (Positive i) = bothWays $ \enc _ ->
  let a' = take (length a) . drop i $ cycle a
  in enc a' k m == enc a k m
prop_negKey (MAK a m k) = bothWays $ \enc dec -> enc a (map negate k) m == dec a k m
prop_differences (MAK a m k) =
  let x = M.fromList $ zip a [0..]
      e = vigenere a k m
      i1 = map (fromJust . flip M.lookup x) m
      i2 = map (fromJust . flip M.lookup x) e
      l = length a
      d = zipWith (\u v -> (u - v) `mod` l) i2 i1
  in and $ zipWith (==) k d

main :: IO ()
main = do
    spectest <- testSpec "Spec" spec
    Test.Tasty.defaultMain $ testGroup "Tests" [
        spectest
      , localOption (QuickCheckTests 1000)
          $ testProperties "properties" props
      ]

spec :: Spec
spec = parallel $ do
    it "should encode" $ do
        vigenere ['a'..'z'] [0..] "abcde" `shouldBe` "acegi"
    it "should decode" $ do
        unvigenere ['a'..'z'] [0..] "acegi" `shouldBe` "abcde"
    it "should encode work with negative key" $ do
        vigenere ['a'..'z'] [-1,-2..] "abcde" `shouldBe` "zzzzz"
    it "should decode work with negative key" $ do
        unvigenere ['a'..'z'] [-1,-2..] "zzzzz" `shouldBe` "abcde"
    it "should encode non-ascii" $ do
        vigenere ['а'..'я'] [0..] "абвгде" `shouldBe` "авджик"
    it "should decode non-ascii" $ do
        unvigenere ['а'..'я'] [0..] "авджик" `shouldBe` "абвгде"
    it "should encode empty message" $ do
        vigenere ['a'..'z'] [0..] "" `shouldBe` ""
    it "should decode empty message" $ do
        unvigenere ['a'..'z'] [0..] "" `shouldBe` ""

props :: [(String, Property)]
props = [
    ("encodes",                                     property prop_encodes)
  , ("vigenere . unvigenere == id",                 property prop_inverse)
  , ("vigenere a [0] m == m",                       property prop_zerokey)
  , ("vigenere a k \"\" == \"\"",                       property prop_emptymsg)
  , ("alphabet cyclic shift doesn't affect result", property prop_shift)
  , ("handles negative key correctly",              property prop_negKey)
  , ("encoded message differences are correct",     property prop_differences)
  ]
