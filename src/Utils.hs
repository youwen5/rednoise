module Utils where

import Constants
import Types

import Data.ByteString.Lazy qualified as LBS
import GHC.IO.Handle (hClose)
import Hakyll
import System.FilePath (
  joinPath,
  splitDirectories,
  takeBaseName,
  takeDirectory,
  takeFileName,
  (</>),
 )
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess, proc, readCreateProcess, readProcess)
import System.Process.Internals
import Templates qualified
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 (Html)

postContext :: Context String
postContext = dateField "date" "%B %e, %Y" <> defaultContext

makeFeed :: Renderer -> Rules ()
makeFeed renderer = do
  route idRoute
  compile $ do
    let feedCtx = postContext <> bodyField "description"
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

makeCompiler' :: (FilePath -> String -> IO String) -> Compiler (Item String)
makeCompiler' f = do
  filePath <- getResourceFilePath
  body <- getResourceBody
  transformed <- unsafeCompiler $ f filePath (itemBody body)
  makeItem transformed

typstProcessor :: FilePath -> String -> IO String
typstProcessor fp content = do
  let dir = takeDirectory fp
  let processSpec = (proc "typst-html-wrapper" []){cwd = Just dir}
  readCreateProcess processSpec content

typstPdfCompiler :: Compiler (Item LBS.ByteString)
typstPdfCompiler = do
  sourcePath <- getResourceFilePath
  pdfContent <- unsafeCompiler $ withSystemTempFile "hakyll-typst.pdf" $ \tempPath handle -> do
    hClose handle
    callProcess "typst" ["compile", sourcePath, tempPath, "--features", "html"]
    LBS.readFile tempPath
  makeItem pdfContent

blazeTemplater ::
  forall t.
  (Context t -> Item t -> Compiler Html) -> Context t -> Item t -> Compiler (Item String)
blazeTemplater template ctx item = do
  compiledHtml <- template ctx item
  makeItem $ renderHtml compiledHtml

tailwindProcessor :: String -> IO String
tailwindProcessor = readProcess "tailwindcss" ["-i", "-", "-o", "-"]

-- take e.g. root/abc.md -> /abc/index.html
toRootHTML :: FilePath -> FilePath
toRootHTML p = takeBaseName p </> "index.html"

-- drop the first parent dir of a path, if it exists
dropFirstParent :: FilePath -> FilePath
dropFirstParent = joinPath . drop 1 . splitDirectories

-- take e.g. dir/abc.md -> dir/abc/index.html
expandRoute :: FilePath -> FilePath
expandRoute p = takeDirectory p </> takeBaseName p </> "index.html"
