module Git where

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
  dateString <- readDate absPath
  return $ parseDate dateString

parseDate :: String -> String
parseDate = defaultFormat . parseTime defaultTimeLocale "%F %T %Z"

defaultFormat :: Maybe UTCTime -> String
defaultFormat = formatTime' "%d.%m.%Y %R"

formatTime' :: String -> Maybe UTCTime -> String
formatTime' _ Nothing = ""
formatTime' format (Just t) = formatTime defaultTimeLocale format t    

readDate :: String -> IO String
readDate file = do
  let cmd = "git"
  let params = ["log", "--pretty=format:%ai", file]
  liftM (take 19) $ readProcess cmd params []  

