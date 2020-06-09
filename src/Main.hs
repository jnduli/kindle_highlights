module Main where


import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as Set
import System.IO  
import           Text.Parsec
import Text.Read
import           Data.Time                      ( getCurrentTime )

import CommandOptions


main :: IO ()
main = do
    options <- getCommandlineOptions
    let fileName = getFilePath options
    let inputType = getInputType options
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let highlights = parse groups "fail" contents
    case highlights of
        Right a -> case inputType of
                KindleList _ -> printUniqueBooks (getUniqueBooks a)
                KindleFilter s -> case readMaybe s :: Maybe Int of
                    Just id -> printKindleHighlights (filterByBookId id a)
                    Nothing -> printKindleHighlights (filterByBookTitle s a)
        Left _ -> print "fail"
    hClose handle

printUniqueBooks :: Set.Set String -> IO ()
printUniqueBooks xs = do
    putStrLn "id\ttitle"
    printZippedUniqueBook $zip [0..] (Set.toList xs)

printZippedUniqueBook :: [(Int, String)] -> IO ()
printZippedUniqueBook (x:xs) = do
    putStrLn (show (fst x) ++ "\t" ++ snd x )
    printZippedUniqueBook xs
printZippedUniqueBook _ = return ()


data KindleHighlight = KindleHighlight String String Integer Integer String deriving (Show, Eq)

getContent:: KindleHighlight -> String
getContent (KindleHighlight _ _ _ _ content) = content

getTitle:: KindleHighlight -> String
getTitle (KindleHighlight title _ _ _ _) = title


getUniqueBooks :: [KindleHighlight] -> Set.Set String
getUniqueBooks [] = Set.empty
getUniqueBooks xs = Set.fromList $L.sort [getTitle x | x <- xs]

-- TODO
-- change ordering
-- For now just order by the first location
-- if k
instance Ord KindleHighlight where
    compare (KindleHighlight t1 _ l1 _ _) (KindleHighlight t2 _ l2 _ _) = compare (t1, l1) (t2, l2)
-- test
-- test = parse remainingGroups "fail" ("\n" ++ exampleGroup ++ eogString )
-- test = parse groups "fail" exampleGroup
-- test = parse location "fail" exampleLocation
--
--
--

-- main = do
--     handle <- openFile "example.txt" ReadMode
--     contents <- hGetContents handle
--     let highlights = parse groups "fail" contents
--     case highlights of
--         Right a -> do
--             let filteredHigh = filterByBookTitle "James P. Carse - Finite and Infinite Games_ A Vision of Life as Play and Possibility-Free Press (1986)" a
--             let sortedHigh = L.sort filteredHigh
--             printKindleHighlights sortedHigh
--             -- print sortedHigh
--         Left _ -> print("fail")
--     hClose handle


printKindleHighlights (x:xs) = do
    putStrLn (getContent x ++ "\n\n")
    printKindleHighlights xs
printKindleHighlights _ = print "\n\n"

filterByBookId :: Int -> [KindleHighlight] -> [KindleHighlight]
filterByBookId id xs = filterByBookTitle (Set.elemAt id (getUniqueBooks xs)) xs


filterByBookTitle :: String -> [KindleHighlight] -> [KindleHighlight]
filterByBookTitle title = filter (checkBookTitle title)

checkBookTitle :: String -> KindleHighlight -> Bool
checkBookTitle a (KindleHighlight title _ _ _ _) = (a == title)

-- csvFile :: GenParser Char st [[String]]
-- csvFile = do
--   result <- many line
--   eof
--   return result

exampleGroup = "Axiomatic (Greg Egan)\n" ++ 
    "- Your Highlight at location 3722-3722 | Added on Sunday, 28 October 2018 08:42:11\n" ++
    "\n" ++
    "mind; maybe some dreams take shape only in the\n" ++ eogString ++ exampleGroup2 ++ eogString

exampleGroup2 = "Axiomatic (Greg Egan)\n" ++ 
    "- Your Highlight at location 3722-3722 | Added on Sunday, 28 October 2018 08:42:11\n" ++
    "\n" ++
    "mind; maybe some dreams take shape only in the\n"

exampleString = "this\nis\ngood\n==========\nanther\ngroup\n=========="

eogString :: String
eogString = "==========\r\n"

groups :: Parsec String st [KindleHighlight]
groups = do
  first <- kindleGroup
  next  <- remainingGroups
  return (first : next)

remainingGroups :: Parsec String st [KindleHighlight]
remainingGroups = groups <|> return []

kindleGroup :: Parsec String st KindleHighlight
kindleGroup = do
    hgh <- highlight 
    endOfGroup
    return hgh

endOfGroup :: Parsec String st String
endOfGroup = string eogString

highlight :: Parsec String st KindleHighlight
highlight = do
    t <- line
    hl <- highlightType
    l <- location
    line
    line
    h <- line
    let x = KindleHighlight (T.unpack (T.strip (T.pack t))) hl (head l) (last l) h
    return x

line :: Parsec String st String
line = manyTill anyChar newline 

highlightType :: Parsec String st String
highlightType = string "- Your " >> manyTill anyChar (char ' ')

location :: Parsec String st [Integer]
location = between (locationStart) (oneOf " |") locationGroupings

locationStart :: Parsec String st String
locationStart = string "at location " <|> string "on page "

getSecondNumner :: Parsec String st Integer
getSecondNumner = do
    char '-'
    end <- many1 digit
    let et = read end :: Integer
    return et


locationGroupings :: Parsec String st [Integer]
locationGroupings = do
    start <- many1 digit
    let st = read start :: Integer
    et <- getSecondNumner <|> return st
    return [st, et]

printTime = do
  time <- getCurrentTime
  print (show time)


-- greet name = "Hello " ++ name ++ "!"

-- main :: IO ()
-- main = do
--   putStrLn $ greet "John"
--   putStrLn $ greet "Mary"
