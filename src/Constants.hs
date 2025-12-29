module Constants where

import Hakyll
import Clay
import System.FilePath ((</>))

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = "Monadic"
    , feedDescription = "Burritos, free software, and related literature"
    , feedAuthorName = "Ananth Venkatesh"
    , feedAuthorEmail = "ananth@monadi.cc"
    , feedRoot = "https://monadi.cc"
    }

topLevel :: [Identifier]
topLevel = ["about.rst", "contact.md"]

snapshotDir :: FilePath
snapshotDir = "content"

postsDir :: FilePath
postsDir = "posts"

templatesDir :: FilePath
templatesDir = "templates"

defaultTemplate :: Identifier
defaultTemplate = fromFilePath $ templatesDir </> "default.html"

archiveTemplate :: Identifier
archiveTemplate = fromFilePath $ templatesDir </> "archive.html"

postTemplate :: Identifier
postTemplate = fromFilePath $ templatesDir </> "post.html"

deepblue :: Color
deepblue = "#004bfe"
