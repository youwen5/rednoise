module Main where

import Compilers
import Constants
import Style
import Templates
import Utils

import Hakyll
import System.FilePath (
  replaceBaseName,
  replaceExtension,
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

    create ["css/style.css"] $ do
      sameRoute
      compile $ makeItem css
    match "css/main.css" $ do
      sameRoute
      compile $ makeCompiler tailwindProcessor

    match "css/*.css" $ do
      sameRoute
      compile compressCssCompiler

    -- match (fromList topLevel) $ do
    --   route $ setExtension "html"
    --   make pandocCompiler

    -- match "posts/*.markdown" $ do
    --   route $ setExtension "html"
    --   build postContext $
    --     pandocCompiler
    --       >>= loadAndApplyTemplate postTemplate postContext
    --       >>= saveSnapshot snapshotDir

    -- match "posts/*.markdown" $ do
    --   route $ setExtension "html"
    --   compile $
    --     pandocCompiler
    --       >>= saveSnapshot snapshotDir
    --       >>= blazeTemplater Templates.defaultTemplate postContext

    match "posts/**.typ" $ do
      -- route $ setExtension "html"
      reroute $ takeFileName . flip replaceExtension "html"
      reroute $ \p ->
        dropFirstParent
          ( takeDirectory p
              </> takeFileName (replaceExtension p "html")
          )
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

    -- match "root/index.html" $ do
    --   reroute takeFileName
    --   compile $ do
    --     posts <- compilePosts
    --     let indexCtx =
    --           listField "posts" postContext (return posts)
    --             `mappend` defaultContext
    --
    --     hydrate indexCtx $ getResourceBody >>= applyAsTemplate indexCtx

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
