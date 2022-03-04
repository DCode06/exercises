{- |
Module                  : Lecture4
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 4 of the Haskell Beginners course.
-}

module Lecture4
    ( -- * Main running function
      main
    , try
    , handler

      -- * Types
    , TradeType (..)
    , Row (..)
    , MaxLen (..)
    , Stats (..)

      -- * Internal functions
    , parseRow
    , splitRow
    , costCheck
    , listLen
    , rowToStats
    , combineRows
    , displayStats
    , calculateStats
    , calc
    , printProductStats
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Max (..), Min (..), Semigroup (..), Sum (..))
import Text.Read (readMaybe)
import Data.Char
import System.Environment
import System.IO
import System.IO.Error
import Data.Maybe

data TradeType
    = Buy
    | Sell
    deriving (Show, Eq, Read)

data Row = Row
    { rowProduct   :: String
    , rowTradeType :: TradeType
    , rowCost      :: Int
    } deriving (Show, Eq)

newtype MaxLen = MaxLen
    { unMaxLen :: String
    } deriving (Show, Eq)

instance Semigroup MaxLen where
    MaxLen x <> MaxLen y = if length x >= length y then MaxLen x else MaxLen y

data Stats = Stats
    { statsTotalPositions :: Sum Int
    , statsTotalSum       :: Sum Int
    , statsAbsoluteMax    :: Max Int
    , statsAbsoluteMin    :: Min Int
    , statsSellMax        :: Maybe (Max Int)
    , statsSellMin        :: Maybe (Min Int)
    , statsBuyMax         :: Maybe (Max Int)
    , statsBuyMin         :: Maybe (Min Int)
    , statsLongest        :: MaxLen
    } deriving (Show, Eq)

instance Semigroup Stats where
    (<>)(Stats totPositions1 totSum1 absMax1 absMin1 sellMax1 sellMin1 buyMax1 buyMin1 long1)(Stats totPositions2 totSum2 absMax2 absMin2 sellMax2 sellMin2 buyMax2 buyMin2 long2)=
	    Stats(totPositions1 <> totPositions2)
        (totSum1 <> totSum2)
        (absMax1 <> absMax2)
        (absMin1 <> absMin2)
        (sellMax1 <> sellMax2)
        (sellMin1 <> sellMin2)
        (buyMax1 <> buyMax2)
        (buyMin1 <> buyMin2)
        (long1 <> long2)

parseRow :: String -> Maybe Row
parseRow "" = Nothing
parseRow rStr = if ((null $ splitRow (== ',') rStr) || ((listLen $ splitRow (== ',') rStr) /= 3)) then Nothing
	            else
                  do
                     let (p:t:c:_) = splitRow (== ',') rStr
                     t <- readMaybe t:: Maybe TradeType
                     c <- costCheck(readMaybe c:: Maybe Int)
                     return (Row { rowProduct = p, rowTradeType = t, rowCost = c})	
            
splitRow :: (Char -> Bool) -> String -> [String]
splitRow p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitRow p s''
                            where (w, s'') = break p s'

costCheck :: Maybe Int -> Maybe Int
costCheck a = if ((fmap (< 0) a)== Just True) then Nothing else a

listLen :: [String] -> Int
listLen [] = 0
listLen ((x:xs):ys) = length $ fmap length ((x:xs):ys)

rowToStats :: Row -> Stats
rowToStats (r@(Row p tt c)) 
    | tt == Buy = Stats { statsTotalPositions = Sum 1, statsTotalSum = Sum (-c), statsAbsoluteMax = Max (c), statsAbsoluteMin = Min (c), statsSellMax = Nothing, statsSellMin = Nothing, statsBuyMax = Just (Max (c)), statsBuyMin = Just (Min (c)), statsLongest = MaxLen (p)}
	| tt == Sell = Stats { statsTotalPositions = Sum 1, statsTotalSum = Sum (c), statsAbsoluteMax = Max (c), statsAbsoluteMin = Min (c), statsSellMax = Just (Max (c)), statsSellMin = Just (Min (c)), statsBuyMax = Nothing, statsBuyMin = Nothing, statsLongest = MaxLen (p)}

combineRows :: NonEmpty Row -> Stats
combineRows (r:|[]) = rowToStats r
combineRows (r:|rs) = let (s:|st) = head (fmap rowToStats (r:rs)):|tail (fmap rowToStats (r:rs)) in sconcat (s:|st)

displayStats :: Stats -> String
displayStats st = "Total positions:       : "++show (getSum (statsTotalPositions st)) ++"\n" ++"Total final balance    : "++ show (getSum(statsTotalSum st))++"\n" ++"Biggest absolute cost  : "++ show (getMax(statsAbsoluteMax st))++"\n" ++"Smallest absolute cost : "++ show (getMin(statsAbsoluteMin st))++"\n" ++"Max earning            : "++ fromMaybe "no value" (fmap show (fmap getMax(statsSellMax st)))++"\n" ++"Min earning            : "++ fromMaybe "no value" (fmap show (fmap getMin(statsSellMin st)))++"\n" ++"Max spending           : "++ fromMaybe "no value" (fmap show (fmap getMax(statsBuyMax st)))++"\n" ++"Min spending           : "++ fromMaybe "no value" (fmap show (fmap getMin(statsBuyMin st)))++"\n" ++"Longest product name   : "++ unMaxLen(statsLongest st)
calculateStats :: String -> String
calculateStats contents = let rows = lines $ contents in calc rows

calc:: [String] -> String
calc (s:ss) = let (r:rs) = mapMaybe parseRow (s:ss) in displayStats $ combineRows (r:|rs)

printProductStats :: FilePath -> IO ()
printProductStats fileName = do
    contents <- readFile fileName
    putStr $ calculateStats contents

main :: IO ()
main = try `catchIOError` handler

try :: IO()
try = do
    (filePath:rest) <- getArgs
    putStrLn filePath
    printProductStats filePath

handler :: IOError -> IO()
handler err
    | isDoesNotExistError err = case ioeGetFileName err of
		    Just path -> putStrLn $ "The file does not exist at "++ path; 
			Nothing   -> putStrLn " The file does not exist at unknown location"
    | isUserError err = putStrLn "Provide file path as command line argument"
	| otherwise = ioError err
