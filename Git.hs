{-# LANGUAGE DeriveDataTypeable #-}

module Git where

import Data.Data
import Data.Typeable
import Data.List
import Data.Ord
import Data.Monoid
import System.FilePath
import System.Locale (TimeLocale, defaultTimeLocale)
import Data.Time.Format (parseTime, formatTime)
import Data.Time.Clock (UTCTime)
import System.Process
import System.Directory
import Control.Monad
  
gitDate :: String -> IO String
gitDate path = do
  dir <- getCurrentDirectory
  let absPath = dir ++ "/" ++ path
  date' <- readDate absPath
  return $ defaultFormat date'

defaultFormat :: Maybe UTCTime -> String
defaultFormat = formatTime' "%d.%m.%Y %R"

formatTime' :: String -> Maybe UTCTime -> String
formatTime' _ Nothing = ""
formatTime' format (Just t) = formatTime defaultTimeLocale format t    

readDate :: String -> IO (Maybe UTCTime)
readDate file = do
  let cmd = "git"
  let params = ["log", "--pretty=format:%ai", file]
  dateString <- liftM (take 19) $ readProcess cmd params []
  return (parseTime defaultTimeLocale "%F %T %Z" dateString)

-------------------
-- HTML page stuff
-------------------
data HtmlPage = HtmlPage { absPath :: String, title:: String, url :: String, date :: Maybe UTCTime} deriving (Data, Typeable, Show)

content :: String -> String -> IO [HtmlPage]
content basedir htmldir = do
  let dir = basedir ++ "/" ++ htmldir
  paths <- getDirectoryContents dir
  let files = filter (isSuffixOf "html") paths
  mapM (\x -> fileInfo (dir ++ "/" ++ x) x x) files

fileInfo :: String -> String-> String -> IO HtmlPage
fileInfo absPath title url = do 
  date <- readDate absPath
  return $ HtmlPage absPath title url date

createUrl :: HtmlPage -> String
createUrl page = "<a href=\"" ++ url' ++ "\">" ++ link ++ "</a>"
  where url'   = (url page)
        title' = (title page)
        date'  = defaultFormat $ (date page)
        link   = title' ++ " (" ++ date' ++ ")"
  
pageList pages = do 
  page <- pages
  let sorted = reverse $ sortBy (comparing date `mappend` comparing title) page
      urls   = map createUrl sorted
      lis    = map (\x -> "<li>" ++ x ++ "</li>") urls      
  return ("<ul>" ++ (join lis) ++ "</ul>")
  