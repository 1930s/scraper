module Main where

import Network.HTTP
import Data.Char
import Data.List
import Text.HTML.TagSoup
import Codec.Binary.UTF8.String (decodeString)

type Title = String
type Torrent = String

data Episode = Episode Title 

instance Show Episode where
  show (Episode title) = title 
  
baseUrl :: String
baseUrl = "http://cn163.net/archives/23809/"

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseTitle :: [Tag String] -> String
parseTitle = innerText . take 2 . dropWhile (~/= "<title>")

parseTorrents :: [Tag String] -> [String]
parseTorrents = filter (isInfixOf "HDTVrip.1024X576") . map f . sections (~== "<a>") . takeWhile (~/= "<div class=back_b>"). dropWhile (~/= "<div id=entry>")
  where f = fromAttrib "href" . head 

main :: IO ()
main = do
    tags <- (parseTags . decodeString) <$> openURL baseUrl
    let title = parseTitle tags
        torrents = parseTorrents tags
    putStrLn $ "torrents: " ++ (unlines torrents)
