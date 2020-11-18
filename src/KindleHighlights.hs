module KindleHighlights where

import qualified Data.Text                     as T
import qualified Data.List                     as L
import qualified Data.Set                      as Set
import           Text.Parsec


-- KindleHighlight data type
-- TODO: Perhaps use record syntax for this
type Title = String
type HighlightType = String
type Content = String
type Location = Integer
data KindleHighlight = KindleHighlight Title HighlightType Location Location Content deriving (Show, Eq)

-- TODO
-- change ordering
-- For now just order by the first location
instance Ord KindleHighlight where
  compare (KindleHighlight t1 _ l1 _ _) (KindleHighlight t2 _ l2 _ _) =
    compare (t1, l1) (t2, l2)

getContent :: KindleHighlight -> String
getContent (KindleHighlight _ _ _ _ content) = content

getTitle :: KindleHighlight -> String
getTitle (KindleHighlight title _ _ _ _) = title

checkBookTitle :: String -> KindleHighlight -> Bool
checkBookTitle a (KindleHighlight title _ _ _ _) = a == title

getUniqueBooks :: [KindleHighlight] -> Set.Set String
getUniqueBooks [] = Set.empty
getUniqueBooks xs = Set.fromList $ L.sort [ getTitle x | x <- xs ]

printUniqueBooks :: Set.Set String -> IO ()
printUniqueBooks xs = do
  putStrLn "id\ttitle"
  printZippedUniqueBook $ zip [0 ..] (Set.toList xs)

printZippedUniqueBook :: [(Int, String)] -> IO ()
printZippedUniqueBook (x : xs) = do
  putStrLn (show (fst x) ++ "\t" ++ snd x)
  printZippedUniqueBook xs
printZippedUniqueBook _ = return ()

printKindleHighlights (x : xs) = do
  putStrLn (getContent x ++ "\n\n")
  printKindleHighlights xs
printKindleHighlights _ = print "\n\n"

filterByBookId :: Int -> [KindleHighlight] -> [KindleHighlight]
filterByBookId id xs = filterByBookTitle (Set.elemAt id (getUniqueBooks xs)) xs

filterByBookTitle :: String -> [KindleHighlight] -> [KindleHighlight]
filterByBookTitle title = filter (checkBookTitle title)


-- Pprsing Methods
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
  t  <- line
  hl <- highlightType
  l  <- location
  line
  line
  h <- line
  let x =
        KindleHighlight (T.unpack (T.strip (T.pack t))) hl (head l) (last l) h
  return x

line :: Parsec String st String
line = manyTill anyChar newline

highlightType :: Parsec String st String
highlightType = string "- Your " >> manyTill anyChar (char ' ')

location :: Parsec String st [Integer]
location = between locationStart (oneOf " |") locationGroupings

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
