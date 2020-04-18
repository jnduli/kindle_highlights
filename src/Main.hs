module Main where



import System.IO  
import           Text.Parsec
import           Data.Time                      ( getCurrentTime )

data KindleHighlight = KindleHighlight String String Integer Integer String deriving (Show, Eq, Ord)

-- test
-- test = parse remainingGroups "fail" ("\n" ++ exampleGroup ++ eogString )
-- test = parse groups "fail" exampleGroup
-- test = parse location "fail" exampleLocation
--
--
--

tests = parse locationStart "fail" "- Your Highlight on page 818-818 | Added on Wednesday, 24 October 2018 04:41:47"

test = do
    handle <- openFile "example.txt" ReadMode
    contents <- hGetContents handle
    let highlights = parse groups "fail" contents
    print highlights
    hClose handle

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
    let x = KindleHighlight t hl (head l) (last l) h
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


greet name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn $ greet "John"
  putStrLn $ greet "Mary"
