{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, mconcat)
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Hakyll
import Git

main :: IO ()
main = hakyllWith config $ do

  match ("images/**" .||. "pdf/*" .||. "js/**" .||. "css/**") $ do
    route   idRoute
    compile copyFileCompiler

  match "templates/*" $ compile $ templateCompiler
  match "menus/*"     $ compile $ getResourceBody
  match "snippets/*"  $ compile $ getResourceBody

  -- FINNISH SITE --
  forM_ ["pages/index.html", "pages/404.shtml"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        leftup <- loadBody "snippets/tapahtumakalenteri.html"
        leftdown <- loadBody "snippets/alkeiskurssi-mainos.html"
        rightup <- loadBody "snippets/harjoitusajat-mainos.html"
        rightdown <- loadBody "snippets/twitter-box.html"
        fiThreeColumn $ fourSnippetCtx leftup leftdown rightup rightdown

  forM_ ["pages/harjoittelu.html", "pages/harjoittelupaikat.html", "pages/katat.html", "pages/perustekniikka.html",
         "pages/salietiketti.html", "pages/graduointi.html", "pages/tyylikuvaus.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-harjoittelu.html"
        fiTwoColumnLeft $ leftCtx menu

  match "pages/leirit.html" $ do
    route setRoot
    compile $ do
      menu <- loadBody "menus/menu-harjoittelu.html"
      right <- loadBody "snippets/leirit-mainos.html"
      fiThreeColumn $ twoSnippetCtx menu right

  forM_ ["pages/alkeiskurssi.html", "pages/alkeiskurssi-ilmo.html", "pages/alkeiskurssi-ilmo-ok.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        right <- loadBody "snippets/alkeiskurssi-ohjelma.html"
        fiTwoColumnRight $ rightCtx right

  forM_ ["pages/yhteystiedot.html", "pages/muutseurat.html", "pages/karate_all.html",
         "pages/jasenmaksut.html", "pages/saannot.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-lisatietoa.html"
        fiTwoColumnLeft $ leftCtx menu

  forM_ ["pages/kuvia.html", "pages/kuvia_07.html", "pages/kuvia_08.html", "pages/kuvia_09.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-kuvia.html"
        fiTwoColumnLeft $ leftCtx menu

  match "pages/muutokset.html" $ do
    route setRoot
    compile $ do
      menu <- loadBody "menus/menu-lisatietoa.html"
      let changes = unsafePerformIO $ pageChanges ["pages"]
      let ctx = mconcat [constField "left" menu, constField "changes" changes, defaultContext]
      applyTemplates "templates/changes.html" fiTemplate ctx

  -- ENGLISH SITE --
  forM_ ["pages/index_en.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        leftup <- loadBody "snippets/tapahtumakalenteri_en.html"
        leftdown <- loadBody "snippets/alkeiskurssi-mainos_en.html"
        rightup <- loadBody "snippets/harjoitusajat-mainos_en.html"
        rightdown <- loadBody "snippets/twitter-box_en.html"
        enThreeColumn $ fourSnippetCtx leftup leftdown rightup rightdown

  forM_ ["pages/harjoittelu_en.html", "pages/harjoittelupaikat_en.html", "pages/katat_en.html", "pages/perustekniikka_en.html",
         "pages/salietiketti_en.html", "pages/graduointi_en.html", "pages/tyylikuvaus_en.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-harjoittelu_en.html"
        enTwoColumnLeft $ leftCtx menu

  match "pages/leirit_en.html" $ do
    route setRoot
    compile $ do
      menu <- loadBody "menus/menu-harjoittelu_en.html"
      right <- loadBody "snippets/leirit-mainos_en.html"
      enThreeColumn $ twoSnippetCtx menu right

  forM_ ["pages/alkeiskurssi_en.html", "pages/alkeiskurssi-ilmo_en.html", "pages/alkeiskurssi-ilmo-ok_en.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        right <- loadBody "snippets/alkeiskurssi-ohjelma_en.html"
        enTwoColumnRight $ rightCtx right

  forM_ ["pages/yhteystiedot_en.html", "pages/muutseurat_en.html", "pages/karate_all_en.html",
         "pages/jasenmaksut_en.html", "pages/saannot_en.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-lisatietoa_en.html"
        enTwoColumnLeft $ leftCtx menu

  forM_ ["pages/kuvia_en.html", "pages/kuvia_07_en.html", "pages/kuvia_08_en.html", "pages/kuvia_09_en.html"] $ \p ->
    match p $ do
      route setRoot
      compile $ do
        menu <- loadBody "menus/menu-kuvia_en.html"
        enTwoColumnLeft $ leftCtx menu

  match "pages/muutokset_en.html" $ do
    route setRoot
    compile $ do
      menu <- loadBody "menus/menu-lisatietoa_en.html"
      let changes = unsafePerformIO $ pageChanges ["pages"]
      let ctx = mconcat [constField "left" menu, constField "changes" changes, defaultContext]
      applyTemplates "templates/changes_en.html" fiTemplate ctx


fiThreeColumn :: Context String -> Compiler (Item String)
fiThreeColumn = applyTemplates "templates/three-column.html" fiTemplate

fiTwoColumnLeft :: Context String -> Compiler (Item String)
fiTwoColumnLeft = applyTemplates "templates/two-column.html" fiTemplate

fiTwoColumnRight :: Context String -> Compiler (Item String)
fiTwoColumnRight = applyTemplates "templates/two-column2.html" fiTemplate

enThreeColumn :: Context String -> Compiler (Item String)
enThreeColumn = applyTemplates "templates/three-column.html" enTemplate

enTwoColumnLeft :: Context String -> Compiler (Item String)
enTwoColumnLeft = applyTemplates "templates/two-column.html" enTemplate

enTwoColumnRight :: Context String -> Compiler (Item String)
enTwoColumnRight = applyTemplates "templates/two-column2.html" enTemplate

fiTemplate :: Item String -> Compiler (Item String)
fiTemplate item = loadAndApplyTemplate "templates/template.html" updatedCtx item >>= relativizeUrls

enTemplate :: Item String -> Compiler (Item String)
enTemplate item = loadAndApplyTemplate "templates/template_en.html" updatedCtx item >>= relativizeUrls

applyTemplates :: Identifier -> (Item String -> Compiler (Item String)) -> Context String -> Compiler (Item String)
applyTemplates columnTemplate pageTemplate ctx =
  getResourceBody
    >>= loadAndApplyTemplate columnTemplate ctx
    >>= pageTemplate

updatedCtx :: Context String
updatedCtx = mconcat
  [ field "updated" $ \item -> do
      return $ unsafePerformIO $ gitDate $ toFilePath $ itemIdentifier item
  , defaultContext
  ]

twoSnippetCtx :: String -> String -> Context String
twoSnippetCtx left right = fourSnippetCtx left "" right ""

fourSnippetCtx :: String -> String -> String -> String -> Context String
fourSnippetCtx leftup leftdown rightup rightdown = mconcat
  [ constField "leftup" leftup
  , constField "leftdown" leftdown
  , constField "rightup" rightup
  , constField "rightdown" rightdown
  , defaultContext
  ]

leftCtx :: String -> Context String
leftCtx left = mconcat [constField "left" left, defaultContext]

rightCtx :: String -> Context String
rightCtx right = mconcat [constField "right" right, defaultContext]

config :: Configuration
config = defaultConfiguration
  { deployCommand = "scp -r _site/* shotofi@shoto.fi:public_html/"
  }

setRoot :: Routes
setRoot = customRoute stripTopDir

stripTopDir :: Identifier -> FilePath
stripTopDir = joinPath . tail . splitPath . toFilePath
