module Backtest.Market (Bar, Ticker, Price, Quantity, priceFromRat, openingPrice, closingPrice, maxPrice, minPrice) where
import Data.Fixed 

type Timestamp = Integer

type CompanyName = String

type Quantity = Integer

type Ticker = String

newtype Price = Price Centi deriving (Eq, Ord, Num)
instance Show Price where show (Price p) = "$" <> show p

priceFromRat :: Rational -> Price
priceFromRat = Price . fromRational

data Bar = Bar
  { dayTimestamp :: Integer,
    closingPrice :: Price,
    openingPrice :: Price,
    maxPrice :: Price,
    minPrice :: Price,
    volume :: Integer,
    name :: CompanyName
  }

mkBar :: Integer -> Price -> Price -> Price -> Price -> Integer -> CompanyName -> Maybe Bar
mkBar ts open close high low volume name
  | low <= min open close && max open close <= high = Just (Bar ts close open high low volume name)
  | otherwise = Nothing
