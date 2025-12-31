module Utils where

import Constants
import Types

import Control.Applicative (optional)
import Hakyll
import System.FilePath (joinPath, splitDirectories, takeBaseName, takeDirectory, (</>))

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

sameRoute :: Rules ()
sameRoute = route idRoute

reroute :: (FilePath -> FilePath) -> Rules ()
reroute f = route $ customRoute $ f . toFilePath

-- take e.g. root/abc.md -> /abc/index.html
toRootHTML :: FilePath -> FilePath
toRootHTML p = takeBaseName p </> "index.html"

-- drop the first parent dir of a path, if it exists
dropFirstParent :: FilePath -> FilePath
dropFirstParent = joinPath . drop 1 . splitDirectories

-- take e.g. dir/abc.md -> dir/abc/index.html
expandRoute :: FilePath -> FilePath
expandRoute p = takeDirectory p </> takeBaseName p </> "index.html"

-- Helper function to extract fields from a Context a, given a corresponding Item a and key.
getField :: Context a -> Item a -> String -> Compiler (Maybe String)
getField ctx item key = optional $ do
  -- unContext looks up the key. The [] is for arguments (usually empty for simple fields)
  field <- unContext ctx key [] item
  -- todo: handle other types of fields
  case field of
    StringField s -> return s
    _ -> fail $ "Field " ++ key ++ " is not a string"
