module Backtest.Execution (Strategy, Order) where
import Backtest.Market(Ticker, Price, Bar, Quantity, priceFromRat)
import Backtest.Order(Fill, executeOrder, filledPrice, Order)

newtype Strategy = Strategy (Bar -> (Maybe Order, Strategy))
data Account = Account { balance :: Price }

applyFill :: Fill -> Account -> (Account)
applyFill fill acc = Account { balance = balance acc - filledPrice fill }

-- Main execution loop
execute :: Strategy -> Bar -> Account -> (Account, Maybe Fill)
execute (Strategy strat) bar acc =
    case fst $ strat bar of
        Just o -> case executeOrder bar o of 
            Just f -> (applyFill f acc, Just f)
            Nothing -> (acc, Nothing)
        Nothing -> (acc, Nothing)
