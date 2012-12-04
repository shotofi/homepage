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

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler
    
    match "snippets/*" $ compile myPageCompiler
    
    match "pages/index.html" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "snippets/tapahtumakalenteri.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/alkeiskurssi-mainos.html" (setFieldA "leftdown" $ arr pageBody)
            >>> requireA "snippets/harjoitusajat.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "rightdown" "")
            >>> arr (setField "breadcrumb" "Pääsivu")
            >>> applyTemplateCompiler "templates/three-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    
config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "scp -r _site/* shotofi@shoto.fi:public_html_test/"
  }

myPageCompiler :: Compiler Resource (Page String)
myPageCompiler = readPageCompiler
  >>> addDefaultFields
  >>> arr applySelf

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
