{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>))

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "gallery/**" $ do
        route   idRoute
        compile copyFileCompiler
 
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "pdf/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

    match (list ["main.html", "harjoitukset.html", "alkeiskurssi.html", "leirit.html", 
            "yleistietoa.html", "muistoja.html", "tyylikuvaus.html", "katat.html", 
            "muumaailma.html", "kds_saannot.html", "karate_all.html", 
            "kuvia_09.html", "kuvia_08.html", "kuvia_07.html", "kuvat1.html", 
            "kuvat2.html", "kuvat3.html", "kuvat4.html", "kuvat5.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler