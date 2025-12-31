module Constants where

import Clay
import Hakyll
import System.FilePath ((</>))

feed :: FeedConfiguration
feed =
  FeedConfiguration
    { feedTitle = "web.youwen.dev"
    , feedDescription = "Youwen www site."
    , feedAuthorName = "Youwen Wu"
    , feedAuthorEmail = "youwen@functor.systems"
    , feedRoot = "https://web.youwen.dev"
    }

topLevel :: [Identifier]
topLevel = ["about.typ", "impressum.typ"]

snapshotDir :: FilePath
snapshotDir = "content"

postsDir :: FilePath
postsDir = "posts"

templatesDir :: FilePath
templatesDir = "templates"

archiveTemplate :: Identifier
archiveTemplate = fromFilePath $ templatesDir </> "archive.html"

postTemplate :: Identifier
postTemplate = fromFilePath $ templatesDir </> "default.html"

deepblue :: Color
deepblue = "#004bfe"
