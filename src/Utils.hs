module Utils where

import Constants
import Types

import Hakyll
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))
import System.Process (readProcess)

postContext :: Context String
postContext =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

makeFeed :: Renderer -> Rules ()
makeFeed renderer = do
  route idRoute
  compile $ do
    let feedCtx = postContext `mappend` bodyField "description"
    posts <-
      fmap (take 10) . recentFirst
        =<< loadAllSnapshots "posts/*" "content"
    renderer feed feedCtx posts

globbify :: FilePath -> Pattern
globbify dir = fromGlob $ dir </> "*"

compilePosts :: Compiler [Item String]
compilePosts = recentFirst =<< (loadAll . globbify) postsDir

hydrate :: Context String -> Compiler (Item String) -> Compiler (Item String)
hydrate context page = page >>= loadAndApplyTemplate defaultTemplate context >>= relativizeUrls

build :: Context String -> Compiler (Item String) -> Rules ()
build context = compile . hydrate context

make :: Compiler (Item String) -> Rules ()
make = compile . hydrate defaultContext

sameRoute :: Rules ()
sameRoute = route idRoute

reroute :: (FilePath -> FilePath) -> Rules ()
reroute f = route $ customRoute $ f . toFilePath

makeCompiler :: (String -> IO String) -> Compiler (Item String)
makeCompiler f = do
  body <- getResourceBody
  transformed <- unsafeCompiler $ f (itemBody body)
  makeItem transformed

typstProcessor :: String -> IO String
typstProcessor = readProcess "typst-html-wrapper" []

tailwindProcessor :: String -> IO String
tailwindProcessor = readProcess "tailwindcss" ["-i", "-", "-o", "-"]

-- take e.g. root/abc.md -> /abc/index.html
toRootHTML :: FilePath -> FilePath
toRootHTML p = takeBaseName p </> "index.html"

-- take e.g. dir/abc.md -> dir/abc/index.html
expandRoute :: FilePath -> FilePath
expandRoute p = takeDirectory p </> takeBaseName p </> "index.html"

-- take e.g. fonts/degheest/fonts/otf/abc.otf -> fonts/degheest/abc.otf
toFontDir :: FilePath -> FilePath
toFontDir p =
  let dir = takeDirectory (takeDirectory (takeDirectory p))
      file = takeFileName p
   in dir </> file

-- take e.g. fonts/lilex/otf/abc.otf -> fonts/lilex/abc.otf
toFontDirSimple :: FilePath -> FilePath
toFontDirSimple p =
  let dir = takeDirectory (takeDirectory p)
      file = takeFileName p
   in dir </> file
