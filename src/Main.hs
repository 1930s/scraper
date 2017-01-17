module Main where

import Network.HTTP
import Data.Char
import Data.List
import Text.HTML.TagSoup
import Codec.Binary.UTF8.String (decodeString)
import Options.Applicative
import Control.Monad
import Debug.Trace

type Title = String
type Torrents = [String]

data Episode = Episode Title Torrents

instance Show Episode where
show (Episode title torrents) = "title\n" ++ unlines torrents

searchUrl :: String -> String
searchUrl keyword = "http://cn163.net/?s=" ++ (urlEncode keyword)

baseUrl :: String
baseUrl = "http://cn163.net/archives/"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseSearch tags = do
  let entries = sections (~== "<div class=entry_box>") . takeWhile (~/= "<div id=pagenavi>") . dropWhile (~/= "<div id=content>") $ tags
  return [(title entry, archive entry) | entry <- entries]
  where title = dropWhile isSpace . innerText . takeWhile (~/= "</h2>") . dropWhile (~/= "<div class=archive_title>")
        archive = fromAttrib "href" . head . tail . takeWhile (~/= "</a>") . dropWhile (~/= "<span class=archive_more>")


parseTitle :: [Tag String] -> String
parseTitle = innerText . take 2 . dropWhile (~/= "<title>")

parseTorrents :: [Tag String] -> [String]
parseTorrents = filter (isInfixOf "HDTVrip.1024X576") . map f . sections (~== "<a>") . takeWhile (~/= "<div class=back_b>"). dropWhile (~/= "<div id=entry>")
  where f = fromAttrib "href" . head 

data Sample = Sample
  { keyword :: String
  , quiet :: Bool }

sample :: Parser Sample
sample = Sample
     <$> strOption
         ( long "keyword"
        <> metavar "TARGET"
        <> help "keyword for torrents" )
     <*> switch
         ( long "quiet"
        <> help "Whether to be quiet" )


greet :: Sample -> IO ()
greet (Sample h False) = putStrLn $ "Hello, " ++ h
greet _ = return ()

cli :: IO ()
cli = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "" )

getTorrents :: IO ()
getTorrents = do
    tags <- (parseTags . decodeString) <$> openURL baseUrl
    let title = parseTitle tags
        torrents = parseTorrents tags
    putStrLn $ "torrents: " ++ (unlines torrents)

testSearch = do
  tags <- (parseTags . decodeString) <$> openURL (searchUrl "生活大爆炸")
  entries <- parseSearch $ tags
  putStrLn . unlines . map showEntry $ entries
  where showEntry (title, archive) = title ++ ": " ++ archive

main = testSearch
