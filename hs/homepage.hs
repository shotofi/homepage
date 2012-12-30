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

    -- FINNISH SITE --
    match "pages/index.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "snippets/tapahtumakalenteri.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/alkeiskurssi-mainos.html" (setFieldA "leftdown" $ arr pageBody)
            >>> requireA "snippets/harjoitusajat-mainos.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "rightdown" "")
            >>> fiThreeColumnCompiler

    forM_ ["pages/harjoittelu.html", "pages/harjoittelupaikat.html", "pages/katat.html", "pages/perustekniikka.html",
           "pages/salietiketti.html", "pages/graduointi.html", "pages/tyylikuvaus.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-harjoittelu.html" (setFieldA "left" $ arr pageBody)
            >>> fiTwoColumnCompiler

    match "pages/leirit.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-harjoittelu.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/leirit-mainos.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "leftdown" "")
            >>> arr (setField "rightdown" "")
            >>> fiThreeColumnCompiler

    forM_ ["pages/alkeiskurssi.html", "pages/alkeiskurssi-ilmo.html", "pages/alkeiskurssi-ilmo-ok.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "snippets/alkeiskurssi-ohjelma.html" (setFieldA "right" $ arr pageBody)
            >>> fiTwoColumnRightCompiler

    forM_ ["pages/yhteystiedot.html", "pages/muutseurat.html", "pages/karate_all.html",
           "pages/jasenmaksut.html", "pages/saannot.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-lisatietoa.html" (setFieldA "left" $ arr pageBody)
            >>> fiTwoColumnCompiler

    forM_ ["pages/kuvia.html", "pages/kuvia_07.html", "pages/kuvia_08.html", "pages/kuvia_09.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-kuvia.html" (setFieldA "left" $ arr pageBody)
            >>> fiTwoColumnCompiler

    match "pages/muutokset.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> arr (setChanges)
            >>> requireA "menus/menu-lisatietoa.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/changes.html"
            >>> fiTwoColumnCompiler

    -- ENGLISH SITE --
    match "pages/index_en.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "snippets/tapahtumakalenteri_en.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/alkeiskurssi-mainos_en.html" (setFieldA "leftdown" $ arr pageBody)
            >>> requireA "snippets/harjoitusajat-mainos_en.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "rightdown" "")
            >>> enThreeColumnCompiler

    forM_ ["pages/harjoittelu_en.html", "pages/harjoittelupaikat_en.html", "pages/katat_en.html", "pages/perustekniikka_en.html",
           "pages/salietiketti_en.html", "pages/graduointi_en.html", "pages/tyylikuvaus_en.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-harjoittelu_en.html" (setFieldA "left" $ arr pageBody)
            >>> enTwoColumnCompiler

    match "pages/leirit_en.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-harjoittelu_en.html" (setFieldA "leftup" $ arr pageBody)
            >>> requireA "snippets/leirit-mainos_en.html" (setFieldA "rightup" $ arr pageBody)
            >>> arr (setField "leftdown" "")
            >>> arr (setField "rightdown" "")
            >>> enThreeColumnCompiler

    forM_ ["pages/alkeiskurssi_en.html", "pages/alkeiskurssi-ilmo_en.html", "pages/alkeiskurssi-ilmo-ok_en.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "snippets/alkeiskurssi-ohjelma_en.html" (setFieldA "right" $ arr pageBody)
            >>> enTwoColumnRightCompiler

    forM_ ["pages/yhteystiedot_en.html", "pages/muutseurat_en.html", "pages/karate_all_en.html",
           "pages/jasenmaksut_en.html", "pages/saannot_en.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-lisatietoa_en.html" (setFieldA "left" $ arr pageBody)
            >>> enTwoColumnCompiler

    forM_ ["pages/kuvia_en.html", "pages/kuvia_07_en.html", "pages/kuvia_08_en.html", "pages/kuvia_09_en.html"] $ \p ->
      match p $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> requireA "menus/menu-kuvia_en.html" (setFieldA "left" $ arr pageBody)
            >>> enTwoColumnCompiler

    match "pages/muutokset_en.html" $ do
        route setRoot
        compile $ historyReadPageCompiler
            >>> arr (setChanges)
            >>> requireA "menus/menu-lisatietoa_en.html" (setFieldA "left" $ arr pageBody)
            >>> applyTemplateCompiler "templates/changes_en.html"
            >>> enTwoColumnCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
  { deployCommand = "scp -r _site/* shotofi@shoto.fi:public_html_test/"
  }

snippetCompiler :: Compiler Resource (Page String)
snippetCompiler = readPageCompiler
  >>> addDefaultFields
  >>> arr applySelf

historyReadPageCompiler :: Compiler Resource (Page String)
historyReadPageCompiler = readPageCompiler
  >>> addDefaultFields
  >>> arr (setGitValues)

-- Finnish compilers
fiTwoColumnCompiler :: Compiler (Page String) (Page String)
fiTwoColumnCompiler = applyTemplateCompiler "templates/two-column.html"
  >>> applyTemplateCompiler "templates/template.html"
  >>> relativizeUrlsCompiler

fiTwoColumnRightCompiler :: Compiler (Page String) (Page String)
fiTwoColumnRightCompiler = applyTemplateCompiler "templates/two-column2.html"
  >>> applyTemplateCompiler "templates/template.html"
  >>> relativizeUrlsCompiler

fiThreeColumnCompiler :: Compiler (Page String) (Page String)
fiThreeColumnCompiler = applyTemplateCompiler "templates/three-column.html"
  >>> applyTemplateCompiler "templates/template.html"
  >>> relativizeUrlsCompiler

-- English compilers
enTwoColumnCompiler :: Compiler (Page String) (Page String)
enTwoColumnCompiler = applyTemplateCompiler "templates/two-column.html"
  >>> applyTemplateCompiler "templates/template_en.html"
  >>> relativizeUrlsCompiler

enTwoColumnRightCompiler :: Compiler (Page String) (Page String)
enTwoColumnRightCompiler = applyTemplateCompiler "templates/two-column2.html"
  >>> applyTemplateCompiler "templates/template_en.html"
  >>> relativizeUrlsCompiler

enThreeColumnCompiler :: Compiler (Page String) (Page String)
enThreeColumnCompiler = applyTemplateCompiler "templates/three-column.html"
  >>> applyTemplateCompiler "templates/template_en.html"
  >>> relativizeUrlsCompiler

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
setChanges page = setField "changes" (unsafePerformIO $ pageChanges ["pages"]) $ page
