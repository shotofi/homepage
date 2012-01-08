{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import System.FilePath
import System.Locale (TimeLocale, defaultTimeLocale)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Control.Monad
import Data.Time.Format (parseTime, formatTime)
import Hakyll
import Hakyll.Core.Compiler

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "gallery/**" $ do
        route   idRoute
        compile copyFileCompiler
 
    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler
    
    -- Tavalliset sivut, joissa voi olla yksi, kaksi tai kolme palstaa
    match (predicate (\i -> matches "pages/*.html" i && not (matches "pages/*_en.html" i))) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr setGitValues
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- Galleria-sivut, joihin tulee vasempaan laitaan lista gallerioista ja loppusivun levyinen 
    -- toinen palsta
    match (predicate (\i -> matches "pages_gallery/*.html" i && not (matches "pages_gallery/*_en.html" i))) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr setGitValues        
            >>> applyTemplateCompiler "templates/gallery.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- ENGLISH VERSION
    match (predicate (\i -> matches "pages/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr setGitValues
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    match (predicate (\i -> matches "pages_gallery/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr setGitValues        
            >>> applyTemplateCompiler "templates/gallery_en.html"
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "scp -r _site/* shotofi@shotofi:public_html/"
  }

setRoot :: Routes
setRoot = customRoute stripTopDir

--stripTopDir :: Identifier -> FilePath -- fix this
stripTopDir = joinPath . tail . splitPath . toFilePath

setGitValues :: Page a -> Page a
setGitValues page = setField "date" gitDate page
  where
    absPath =  "/Users/jvesala/Git/homepage/" ++ (getField "path" page)
    gitDate = unsafePerformIO $ readDate absPath

readDate :: String -> IO String
readDate file = do
  let cmd = "git"
  let params = ["log", "--pretty=format:%ai", file]
  liftM (take 19) $ readProcess cmd params []