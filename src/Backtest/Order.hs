module Backtest.Order (Order, OrderType, Side, Fill, executeOrder, filledPrice) where 
import Backtest.Market ( Ticker, Price, Bar(openingPrice), Quantity, maxPrice, minPrice)

data Side = Buy | Sell deriving (Eq, Show)

data OrderType =
    MarketType
    | Market
    | Limit Price
    | Stop Price
    | StopLimit Price Price
    deriving (Eq, Show)

data Order = Order {
    orderType   :: OrderType,
    side        :: Side,
    ticker      :: Ticker,
    quantity    :: Quantity
    } deriving (Show)

data Fill = Fill { filledPrice :: Price }

executeOrder :: Bar -> Order -> Maybe Fill
executeOrder bar order = case orderLogic bar (side order) (orderType order) of 
    Just f -> Just Fill { filledPrice = filledPrice f + applyCosts order }
    Nothing -> Nothing

-- Execution is filling an order based on the current Bar
orderLogic :: Bar -> Side -> OrderType -> Maybe Fill
orderLogic bar Buy (MarketType) = Just $ Fill { filledPrice = openingPrice bar}
orderLogic bar Sell (MarketType) = Just $ Fill { filledPrice = openingPrice bar }

orderLogic bar Buy (Limit l) = if minPrice bar < l then Just Fill { filledPrice = (min l (openingPrice bar)) } else Nothing
orderLogic bar Sell (Limit l) = if maxPrice bar > l then Just Fill { filledPrice = (max l (openingPrice bar)) } else Nothing

orderLogic bar Buy (Stop s) = if maxPrice bar > s then Just Fill { filledPrice = max s (openingPrice bar) } else Nothing
orderLogic bar Sell (Stop s) = if minPrice bar < s then Just Fill { filledPrice = min s (openingPrice bar) } else Nothing

orderLogic bar side (StopLimit s l) = if trigger then orderLogic bar side (Limit l) else Nothing
    where trigger = case side of 
            Buy -> maxPrice bar > s
            Sell -> minPrice bar < s

-- TODO: Costs based on order
applyCosts :: Order -> Price
applyCosts _ = 0