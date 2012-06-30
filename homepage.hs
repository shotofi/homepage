{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Hakyll
import Hakyll.Core.Compiler
import Git

main :: IO ()
main = hakyllWith config $ do
    match "images/**" $ do
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
            >>> arr (setGitValues)
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- Galleria-sivut, joihin tulee vasempaan laitaan lista gallerioista ja loppusivun levyinen 
    -- toinen palsta
    match (predicate (\i -> matches "pages_gallery/*.html" i && not (matches "pages_gallery/*_en.html" i))) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> applyTemplateCompiler "templates/gallery.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- ENGLISH VERSION
    match (predicate (\i -> matches "pages/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    match (predicate (\i -> matches "pages_gallery/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> applyTemplateCompiler "templates/gallery_en.html"
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    -- CHANGES PAGES
    match "pages_changes/muutokset.html" $ do 
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)            
            >>> arr (setChanges)        
            >>> applyTemplateCompiler "templates/changes.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    match "pages_changes/muutokset_en.html" $ do 
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)            
            >>> arr (setChanges)        
            >>> applyTemplateCompiler "templates/changes.html"
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    
config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "scp -r _site/* shotofi@shoto.fi:public_html/"
  }

setRoot :: Routes
setRoot = customRoute stripTopDir

stripTopDir :: Identifier a -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath

setGitValues :: Page a -> Page a
setGitValues page = setField "updated" (unsafePerformIO $ gitDate $ getField "path" page) $ page

setGitValues' :: Page a -> IO (Page a)
setGitValues' page = liftM (setUpdated page) $ (gitDate $ getField "path" page)
  where setUpdated page date = setField "updated" date page
  
setChanges :: Page a -> Page a
setChanges page = setField "changes" (unsafePerformIO $ pageChanges ["pages", "pages_gallery"]) $ page
