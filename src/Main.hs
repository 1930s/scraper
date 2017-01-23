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
type Archive = String

searchUrl :: String -> String
searchUrl keyword = "http://cn163.net/?s=" ++ (urlEncode keyword)

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

parseSearch tags = do
  let entries = sections (~== "<div class=entry_box>") tags
  return [(title entry, archive entry) | entry <- entries]
  where title = dropWhile isSpace . innerText . takeWhile (~/= "</h2>") . dropWhile (~/= "<div class=archive_title>")
        archive = fromAttrib "href" . head . tail . takeWhile (~/= "</a>") . dropWhile (~/= "<span class=archive_more>")


parseTitle :: [Tag String] -> String
parseTitle = innerText . take 2 . dropWhile (~/= "<title>")

parseTorrents :: [Tag String] -> [String]
parseTorrents = filter (isInfixOf "1024") . map f . sections (~== "<a>") . takeWhile (~/= "<div class=back_b>"). dropWhile (~/= "<div id=entry>")
  where f = fromAttrib "href" . head 

getTorrents :: String -> IO ()
getTorrents archiveUrl = do
    tags <- (parseTags . decodeString) <$> openURL archiveUrl
    let title = parseTitle tags
        torrents = parseTorrents tags
    putStrLn $ "torrents:\n " ++ (unlines torrents)
  
search keyword = do
  tags <- (parseTags . decodeString) <$> openURL (searchUrl keyword)
  entries <- parseSearch $ tags
  return $ zip entries [0..]

showEntry ((title, _), index) = (show index) ++ ") " ++ title

data Params = Params
  { keyword :: String}

keywordParser :: Parser Params
keywordParser = Params
  <$> strOption
      (long "keyword"
     <> metavar "TARGET"
     <> help "keyword for video")

main :: IO ()
main = do
  keyword <- execParser $ info keywordParser mempty
  case keyword of
    Params key -> do
      entries <- search key
      putStrLn . unlines . map showEntry $ entries
      putStr "which one: "
      which <- getLine
      let choose = entries !! (read which)
      getTorrents $ snd . fst $ choose
      
