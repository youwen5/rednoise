module Main where

import Compilers
import Constants
import Style
import Templates
import Utils

import Hakyll
import System.FilePath (
  dropExtension,
  replaceBaseName,
  replaceExtension,
  takeBaseName,
  takeDirectory,
  takeFileName,
  (</>),
 )
import Text.Blaze.Html.Renderer.String

main :: IO ()
main = generateSite

generateSite :: IO ()
generateSite =
  hakyll $ do
    match "images/*" $ do
      sameRoute
      compile copyFileCompiler

    match "static/**" $ do
      sameRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      sameRoute
      compile copyFileCompiler

    match "root/favicon.ico" $ do
      reroute takeFileName
      compile copyFileCompiler

    match "css/main.css" $ do
      sameRoute
      compile $ do
        makeCompiler tailwindProcessor

    match "css/*.css" $ do
      sameRoute
      compile compressCssCompiler

    match "posts/**.typ" $ do
      -- route $ setExtension "html"
      reroute $
        dropFirstParent
          . (</> "index.html")
          . dropExtension

      compile $
        typstHtmlCompiler postContext
          >>= saveSnapshot snapshotDir
          >>= blazeTemplater Templates.postTemplate postContext

    create ["archive.html"] $ do
      reroute expandRoute
      compile $ do
        posts <- loadAll "posts/**"
        let archiveCtx =
              listField postsDir postContext (return posts)
                <> constField "title" "Archives"
                <> defaultContext
        makeItem ""
          >>= blazeTemplater Templates.archivePage archiveCtx
          >>= blazeTemplater Templates.wideTemplate archiveCtx

    match "root/index.typ" $ do
      reroute $ takeFileName . flip replaceExtension "html"
      compile $
        typstIndexCompiler defaultContext
          >>= blazeTemplater indexTemplate defaultContext

    match ("cv/index.typ" .||. "cv/short.typ") $ do
      route $ setExtension "html"
      compile $
        typstHtmlCompiler defaultContext
          >>= blazeTemplater Templates.defaultTemplate defaultContext

    match "cv/index.typ" $ version "pdf" $ do
      reroute $ \p ->
        takeDirectory p
          </> ( (takeFileName . flip replaceBaseName "youwen-wu-cv-full")
                  . (takeFileName . flip replaceExtension "pdf")
              )
            p
      compile typstPdfCompiler

    match "cv/short.typ" $ version "pdf" $ do
      reroute $ \p ->
        takeDirectory p
          </> ( (takeFileName . flip replaceBaseName "youwen-wu-cv-short")
                  . (takeFileName . flip replaceExtension "pdf")
              )
            p
      compile typstPdfCompiler

    match "root/*.typ" $ do
      reroute toRootHTML
      compile $
        typstHtmlCompiler defaultContext
          >>= blazeTemplater Templates.defaultTemplate defaultContext

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ makeFeed renderAtom
    create ["feed.xml"] $ makeFeed renderRss
    create ["feed.json"] $ makeFeed renderJson
