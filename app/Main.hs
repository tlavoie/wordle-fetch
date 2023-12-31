{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Data.Maybe (fromJust)
import Data.Text
import Network.Wreq
import qualified Network.Wreq.Session as S
import Control.Lens
import Data.Aeson.Lens (_String, key, _Integer)

-- libraries needed (or anticipated) for concurrency and parallelism
import Control.Parallel.Strategies
import Control.Concurrent.Async

dateOffset :: UTCTime -> Integer -> String
dateOffset date dayOffset =
  formatTime defaultTimeLocale "%Y-%m-%d" $ addUTCTime (nominalDay * fromInteger dayOffset) date

getWordleOfDay :: S.Session -> [Char] -> IO [Char]
getWordleOfDay session dateStr = do
  start <- Data.Time.getCurrentTime
  r <- S.get session ("https://www.nytimes.com/svc/wordle/v2/" ++ dateStr ++ ".json")
  end <- Data.Time.getCurrentTime

  let someid = show $ fromJust (r ^? responseBody . key "id" . _Integer)
  let solution = Data.Text.unpack $ r ^. responseBody . key "solution" . _String
  let millis = show $ (floor $ 1000 * diffUTCTime end start :: Integer)
  return $ someid ++ "," ++ solution ++ "," ++ dateStr ++ "," ++ millis
  
main :: IO ()
main = do
  let dateStartString = "2021-06-19"
  let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStartString :: UTCTime
  let range = [0 .. 680] :: [Integer] -- Range to match Steve's example

  -- Not sure if this CPU parallelization buys much, not that costly
  let dateStrList = parMap rdeepseq (dateOffset startDate) range
  mainStart <- Data.Time.getCurrentTime
  sess <- S.newSession

  result <- mapConcurrently (getWordleOfDay sess) dateStrList

  print result
  mainEnd <- Data.Time.getCurrentTime
  putStr "main time, in ms: "
  putStrLn $ show ( floor $ 1000 * diffUTCTime mainEnd mainStart :: Integer)

  
  
