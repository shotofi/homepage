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
    match "menus/*"     $ compile snippetCompiler
    match "snippets/*"  $ compile snippetCompiler
    
    match "pages/index.html" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "snippets/tapahtumakalenteri.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/alkeiskurssi-mainos.html" (setFieldA "leftdown" $ arr pageBody)
            >>> requireA "snippets/harjoitusajat.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "rightdown" "")
            >>> applyTemplateCompiler "templates/three-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    forM_ ["pages/harjoittelu.html", "pages/katat.html", "pages/perustekniikka.html",
           "pages/tyylikuvaus.html", "pages/lajinvalinta.html", "pages/muiden_harrastajat.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "menus/menu-harjoittelu.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/two-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    match "pages/leirit.html" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "menus/menu-harjoittelu.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/leirit-mainos.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "leftdown" "")
            >>> arr (setField "rightdown" "")
            >>> applyTemplateCompiler "templates/three-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    match "pages/alkeiskurssi.html" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "snippets/alkeiskurssi-ohjelma.html" (setFieldA "right" $ arr pageBody)
            >>> applyTemplateCompiler "templates/two-column2.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    forM_ ["pages/yhteystiedot.html", "pages/muumaailma.html", "pages/karate_all.html",
           "pages/jasenmaksut.html", "pages/saannot.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "menus/menu-yhteystiedot.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/two-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    match "pages_gallery/*" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> requireA "menus/menu-muistoja.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/two-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- CHANGES PAGES
    match "pages/muutokset.html" $ do
        route setRoot
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr (setGitValues)
            >>> arr (setChanges)
            >>> requireA "menus/menu-yhteystiedot.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/changes.html"
            >>> applyTemplateCompiler "templates/two-column.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "scp -r _site/* shotofi@shoto.fi:public_html_test/"
  }

snippetCompiler :: Compiler Resource (Page String)
snippetCompiler = readPageCompiler
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
