
module Backtest.OrderSpec (spec) where

import Test.Hspec

import Data.Time (fromGregorian)
import Data.Either (isLeft)
import Backtest.Order

genBar :: Gen Bar
genBar = do
    ts      <- choose (0, 100000)
    -- The prices are in cents
    low     <- choose (0.1, 10000)
    hi      <- choose (low + 0.1, low + 200)
    open    <- choose (low, high)
    close   <- choose (low, high)
    n       <- elements ["NVIDIA", "GOOGL", "AAPL", "MSFT", "TEST"]
    let getPrice x = priceFromRat (toRational x / 100) -- Conversion to Price from integer cents
    case mkBar ts (toPrice open) (toPrice close) (toPrice high) (toPrice low) n of
        Just bar -> pure bar
        Nothing -> error "generated invalid bar"


spec :: Spec
spec = do