module Backtest.Parser where
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.List (dropWhileEnd)
import Data.Char ( isSpace ) 

data RawBar = RawBar { rDate :: Day, rOpen :: Double, rHigh :: Double, rLow :: Double, rClose :: Double, rVolume :: Int } deriving(Show, Eq)

splitOn :: String -> Char -> [String]
splitOn s c = case splitted of 
  (part, [])      -> [part]
  (part, _:rest)  -> part : splitOn rest c
  where splitted = break (\t -> t == c) s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

field :: Read a => String -> String -> Either String a
field label str  = case reads (trim str) of
  [(x, "")] -> Right x
  _         -> Left ("invalid label " ++ label ++ ": " ++ show str)

day :: String -> Either String Day
day s = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (trim s) of
  Just d -> Right d
  Nothing -> Left ("invalid date" ++ show s)

parseLine :: String -> Either String RawBar
parseLine line = case splitOn line ',' of
  [d, o, h, l, c, v] ->
    RawBar  <$> day d
            <*> field "open"    o
            <*> field "high"    h
            <*> field "low"     l
            <*> field "close"   c
            <*> field "volume"  v

