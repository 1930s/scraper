module Main where

import Network.HTTP
import Data.Char
import Data.List
import Text.HTML.TagSoup
import Codec.Binary.UTF8.String (decodeString)
import Options.Applicative
import Control.Monad

import Debug.Trace

searchUrl :: String -> String
searchUrl keyword = "http://cn163.net/?s=" ++ (urlEncode keyword)

openURL :: String -> IO String
openURL x = do
  response <- simpleHTTP (getRequest x)
  code <- getResponseCode response
  putStrLn ("statusCode: " ++ (show code))
  body <- getResponseBody response
  return body

parseSearch :: [Tag String] -> [(String, String)]
parseSearch = map (\c -> (title c, archive c)) . sections (~== "<div class=entry_box>")
  where title = dropWhile isSpace . innerText . takeWhile (~/= "</h2>") . dropWhile (~/= "<div class=archive_title>")
        archive = fromAttrib "href" . head . tail . takeWhile (~/= "</a>") . dropWhile (~/= "<span class=archive_more>")


parseTorrents :: [Tag String] -> [String]
parseTorrents = filter (isInfixOf "1024") . map f . sections (~== "<a>") . takeWhile (~/= "<div class=back_b>"). dropWhile (~/= "<div id=entry>")
  where f = fromAttrib "href" . head 

getTorrents :: String -> IO ()
getTorrents archiveUrl = do
    tags <- (parseTags . decodeString) <$> openURL archiveUrl
    let torrents = parseTorrents tags
    putStrLn $ "torrents:\n " ++ (unlines torrents)

search :: String -> IO [(String, String)]
search keyword = do
  tags <- (parseTags . decodeString) <$> openURL (searchUrl keyword)
  let entries = parseSearch $ tags
  putStrLn . unlines . map showEntry . flip zip [0..] $ entries
  putStrLn "which one: "
  return entries

showEntry :: ((String, String), Integer) -> String
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
      which <- getLine
      getTorrents . snd $ (entries !! (read which))
