module Main where

import           System.IO
import           Text.Parsec
import           Text.Read

import           CommandOptions
import           KindleHighlights


main :: IO ()
main = do
  options <- getCommandlineOptions
  let fileName  = getFilePath options
  let inputType = getInputType options
  handle   <- openFile fileName ReadMode
  contents <- hGetContents handle
  let highlights = parse groups "fail" contents
  case highlights of
    Right a -> case inputType of
      KindleList   _ -> printUniqueBooks (getUniqueBooks a)
      KindleFilter s -> case readMaybe s :: Maybe Int of
        Just id -> printKindleHighlights (filterByBookId id a)
        Nothing -> printKindleHighlights (filterByBookTitle s a)
    Left _ -> print "fail"
  hClose handle



