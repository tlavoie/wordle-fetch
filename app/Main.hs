{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time
import Data.DateTime
import Data.Maybe (fromJust)
import Data.Text
import Network.Wreq
-- import qualified Network.Wreq.Session as S
import Control.Lens
--import Data.Aeson (toJSON)
import Data.Aeson.Lens (_String, key, _Integer)
--import qualified Data.Text as T

dateOffset :: DateTime -> Integer -> String
dateOffset date dayOffset =
  formatTime defaultTimeLocale "%Y-%m-%d" $ addMinutes (dayOffset * 1440) date

getWordleOfDay :: [Char] -> IO [Char]
getWordleOfDay dateStr = do
  start <- Data.Time.getCurrentTime
  r <- get ("https://www.nytimes.com/svc/wordle/v2/" ++ dateStr ++ ".json")
  end <- Data.Time.getCurrentTime

  let someid = show $ fromJust (r ^? responseBody . key "id" . _Integer)
  let solution = Data.Text.unpack $ r ^. responseBody . key "solution" . _String
  let millis = show $ (floor $ 1000 * diffUTCTime end start :: Integer)
  return $ someid ++ "," ++ solution ++ "," ++ dateStr ++ "," ++ millis
  
main :: IO ()
main = do
  let dateStartString = "2021-06-19"
  let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateStartString :: UTCTime
  let range = [0 .. 9]  -- sample range
  -- let range = [0 .. 680]  -- full range
  let dateStrList = Prelude.map (dateOffset startDate) range
  mainStart <- Data.Time.getCurrentTime
  -- result <- getWordleOfDay dateStartString
  result <- mapM getWordleOfDay dateStrList
  print result
  -- Prelude.map print Prelude.map getWordleOfDay dateStrList
  mainEnd <- Data.Time.getCurrentTime
  putStr "main time, in ms: "
  putStrLn $ show ( floor $ 1000 * diffUTCTime mainEnd mainStart :: Integer)

  
  
