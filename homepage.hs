{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (arr, (>>>))

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

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler
    
    -- Tavalliset sivut, joissa voi olla yksi, kaksi tai kolme palstaa
    match (list ["main.html", "harjoitukset.html", "alkeiskurssi.html", "leirit.html", 
            "yleistietoa.html","tyylikuvaus.html", "katat.html", "muutokset.html",
            "muumaailma.html", "kds_saannot.html", "karate_all.html",
            "yhteystiedot.html", "lajinvalinta.html", "muiden_harrastajat.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler

    -- Galleria-sivut, joihin tulee vasempaan laitaan lista gallerioista ja loppusivun levyinen 
    -- toinen palsta
    match (list ["muistoja.html", "kuvia_09.html", "kuvia_08.html", "kuvia_07.html", "kuvat1.html",
            "kuvat2.html", "kuvat3.html", "kuvat4.html", "kuvat5.html", 
            "kuvat6.html", "kuvat7.html", "kuvat8.html", "kuvat9.html", "kuvat10.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/gallery.html"
            >>> applyTemplateCompiler "templates/template.html"
            >>> relativizeUrlsCompiler


    -- ENGLISH VERSION
    match (list ["english.html", "harjoitukset_en.html", "alkeiskurssi_en.html", "leirit_en.html", 
            "yleistietoa_en.html","tyylikuvaus_en.html", "katat_en.html", "muutokset_en.html",
            "muumaailma_en.html", "karate_all_en.html",
            "yhteystiedot_en.html", "lajinvalinta_en.html", "muiden_harrastajat_en.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler

    match (list ["muistoja_en.html", "kuvia_09_en.html", "kuvia_08_en.html", "kuvia_07_en.html", "kuvat1_en.html",
            "kuvat2_en.html", "kuvat3_en.html", "kuvat4_en.html", "kuvat5_en.html", 
            "kuvat6_en.html", "kuvat7_en.html", "kuvat8_en.html", "kuvat9_en.html", "kuvat10_en.html"]) $ do
        route   $ setExtension "html"
        compile $ readPageCompiler
            >>> applyTemplateCompiler "templates/gallery_en.html"
            >>> applyTemplateCompiler "templates/template_en.html"
            >>> relativizeUrlsCompiler    