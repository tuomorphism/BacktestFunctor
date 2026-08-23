module Costs (applyCosts) where
import Market (Price)
import Order (Order)
import Data.Fixed   

applyCosts :: Order -> Price
applyCosts x = 0