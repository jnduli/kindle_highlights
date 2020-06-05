module CommandOptions where

import Options.Applicative
import Data.Semigroup ((<>))


data Input = Input String InputType deriving Show


getFilePath :: Input -> String
getFilePath (Input f _) = f

getInputType :: Input -> InputType
getInputType (Input _ x) = x

data InputType = KindleFilter String | KindleList Bool deriving Show

commandInput :: Parser Input
commandInput = Input <$> strOption ( long "file" <> short 'f' <> help "File to check") <*> (filterInput <|> listInput)

filterInput :: Parser InputType
filterInput = KindleFilter <$> strOption ( long "filter" <> help "Filter by name provided")

listInput :: Parser InputType
listInput = KindleList <$> switch ( long "list" <> short 'l' <> help "Show books in file")

opts :: ParserInfo Input 
opts = info (commandInput <**> helper)
  ( fullDesc
  <> progDesc "Parse kindle highlights file"
  <> header "kindle highligts - a basic parser" )


getCommandlineOptions:: IO Input
getCommandlineOptions = execParser opts
