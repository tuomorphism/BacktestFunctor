module Backtest.Parser (loadBarData) where
import Data.Time (Day)

data RawBar = RawBar { rDate :: Day, rOpen :: Double, rHigh :: Double, rLow :: Double, rClose :: Double, rVolume :: Int }


instance FromNamedRecord RawBar where
  parseNamedRecord m = RawBar
    <$> m .: "Date" <*> m .: "Open"  <*> m .: "High"
    <*> m .: "Low"  <*> m .: "Close" <*> m .: "Volume"

instance FromField Day where
  parseField = maybe (fail "bad date") pure
             . parseTimeM True defaultTimeLocale "%Y-%m-%d"
             . B8.unpack

