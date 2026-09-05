module Backtest.ParserSpec (spec) where

import Test.Hspec

import Data.Time (fromGregorian)
import Data.Either (isLeft)
import Backtest.Parser

spec :: Spec
spec = do
    describe "day" $ do
        it "parsing an iso date" $
            day "2024-01-15" `shouldBe` Right (fromGregorian 2024 1 15)
        
        it "non-iso date should be rejected" $
            day "2025/01-13" `shouldSatisfy` isLeft

        it "should handle whitespace" $ 
            day "  2025-03-25  " `shouldBe` Right (fromGregorian 2025 3 25)

    describe "line parser" $ do
        it "should parse a valid line" $
            parseLine "2025-01-13,100.0,102.0,98.6,101.17,1230" `shouldBe` Right (RawBar (fromGregorian 2025 1 13) 100 102 98.6 101.17 1230)
        
        it "should not parse an invalid line" $
            parseLine "2025-01-13,100.0,102.0,,101.17,1230" `shouldSatisfy` isLeft
        
        it "should have message of empty field" $
            case parseLine "2025-01-13,100.0,102.0,,101.17,1230" of
                Left s  -> s `shouldContain` "low"
                Right _ -> expectationFailure "expected a Left type"
