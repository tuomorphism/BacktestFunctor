module Market () where

type Timestamp = Integer

type CompanyName = String

type Price = Float

data DailyTicker = DailyTicker
  { dayTimestamp :: Integer,
    closingPrice :: Price,
    openingPrice :: Price,
    name :: CompanyName
  }

data Ticker = Ticker [DailyTicker]

data Market = Market [Ticker]

dayProfit :: Ticker -> [Float]
dayProfit (Ticker []) = []
dayProfit (Ticker (s : rest)) =
  [((closingPrice s) - (openingPrice s)) / openingPrice s] ++ dayProfit (Ticker rest)
