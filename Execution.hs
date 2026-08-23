module Execution (Strategy, Order) where
import Market(Ticker, Price, Bar, Quantity, priceFromRat)
import Order(Fill, executeOrder, filledPrice, Order)
import Costs(applyCosts)

newtype Strategy = Strategy (Bar -> (Maybe Order, Strategy))
data Account = Account { balance :: Price }

applyOrder :: Bar -> Order -> Account -> (Account, Maybe Fill)
applyOrder bar order acc =
    case fill of 
        Just f -> (Account { balance = balance acc - filledPrice f }, fill)
        Nothing -> (acc, Nothing)
    where fill = executeOrder bar order

-- Main execution loop
execute :: Strategy -> Bar -> Account -> (Account, Maybe Fill)
execute (Strategy strat) bar acc =
    case order of
        Just o -> applyOrder bar o acc
        Nothing -> (acc, Nothing)
    where order = fst $ strat bar