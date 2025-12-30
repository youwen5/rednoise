module Main where

import Constants
import Style
import Utils

import Data.Monoid (mappend)
import Hakyll
import System.FilePath (replaceExtension, takeFileName)

main :: IO ()
main = generateSite

generateSite :: IO ()
generateSite = hakyll $ do
  match "images/*" $ do
    sameRoute
    compile copyFileCompiler

  match "root/favicon.*" $ do
    reroute takeFileName
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

  match (fromList topLevel) $ do
    route $ setExtension "html"
    make pandocCompiler

  match "posts/*.markdown" $ do
    route $ setExtension "html"
    build postContext $
      pandocCompiler
        >>= loadAndApplyTemplate postTemplate postContext
        >>= saveSnapshot snapshotDir

  match "posts/*.typ" $ do
    route $ setExtension "html"
    compile $
      makeCompiler' typstProcessor
        >>= loadAndApplyTemplate postTemplate postContext
        >>= saveSnapshot snapshotDir

  create ["archive.html"] $ do
    reroute expandRoute
    compile $ do
      posts <- compilePosts
      let archiveCtx =
            listField postsDir postContext (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      hydrate archiveCtx $ makeItem "" >>= loadAndApplyTemplate archiveTemplate archiveCtx

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
      makeCompiler' typstProcessor
        >>= loadAndApplyTemplate indexTemplate defaultContext

  match ("cv/index.typ" .||. "cv/short.typ") $ do
    route $ setExtension "html"
    make (makeCompiler' typstProcessor)

  match ("cv/index.typ" .||. "cv/short.typ") $ version "pdf" $ do
    route $ setExtension "pdf"
    compile typstPdfCompiler

  match "root/*.typ" $ do
    reroute toRootHTML
    make $ makeCompiler' typstProcessor

  match "templates/*" $ compile templateBodyCompiler

  create ["atom.xml"] $ makeFeed renderAtom
  create ["feed.xml"] $ makeFeed renderRss
  create ["feed.json"] $ makeFeed renderJson
