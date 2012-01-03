{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))
import System.FilePath
import Hakyll

main :: IO ()
main = hakyll $ do
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
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- Galleria-sivut, joihin tulee vasempaan laitaan lista gallerioista ja loppusivun levyinen 
    -- toinen palsta
    match (predicate (\i -> matches "pages_gallery/*.html" i && not (matches "pages_gallery/*_en.html" i))) $ do
        route setRoot
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/gallery.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- ENGLISH VERSION
    match (predicate (\i -> matches "pages/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    match (predicate (\i -> matches "pages_gallery/*_en.html" i)) $ do
        route setRoot
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/gallery_en.html"
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

setRoot :: Routes
setRoot = customRoute stripTopDir

--stripTopDir :: Identifier -> FilePath -- fix this
stripTopDir = joinPath . tail . splitPath . toFilePath
