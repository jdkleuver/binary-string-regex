import Test.Hspec
import Test.QuickCheck
import Text.Regex.TDFA
import Numeric
import Data.Char
import BinaryRegex (regexDivisibleBy)

test_n :: Int -> Int -> Spec
test_n n max_n = do
    let test | n == max_n = do
                            let regex_n = regexDivisibleBy n
                            it "edge cases" $ do
                              regex_n `shouldSatisfy`(not . null)
                              ""  =~ regex_n `shouldBe` False
                              "0" =~ regex_n `shouldBe` True
                            it ("divisible by " ++ (show n)) $ do
                                withMaxSuccess 5 $ property $ forAll (choose (1::Int, 2^31-1)) $ \i -> do -- 2^31-1
                                   showIntAtBase 2 intToDigit i "" =~ regex_n `shouldBe` i `mod` n==0
             | otherwise = do
                           let regex_n = regexDivisibleBy n
                           it "edge cases" $ do
                             regex_n `shouldSatisfy`(not . null)
                             ""  =~ regex_n `shouldBe` False
                             "0" =~ regex_n `shouldBe` True
                           it ("divisible by " ++ (show n)) $ do
                               withMaxSuccess 5 $ property $ forAll (choose (1::Int, 2^31-1)) $ \i -> do
                                   showIntAtBase 2 intToDigit i "" =~ regex_n `shouldBe` i `mod` n==0
                           test_n (n+1) max_n
    test
     
      
main = hspec $ test_n 1 16
