{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "gallery/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "gallery/thumbs/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "gallery/videos/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    match (list ["main.html", "harjoitukset.html", "alkeiskurssi.html", "leirit.html", 
            "yleistietoa.html", "muistoja.html", "tyylikuvaus.html", "katat.html", 
            "muumaailma.html", "kds_saannot.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler