{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates where

import BlazeSupport
import Control.Monad (forM_, when)
import Data.ByteString.Lazy
import Data.Maybe
import Hakyll
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Utils

pageHead :: Maybe String -> Maybe String -> Maybe String -> Html
pageHead title pagetitle slug =
  H.head $
    do
      meta ! charset "utf-8"
      meta ! httpEquiv "x-ua-compatible" ! content "ie=edge"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      H.title $ forM_ title toHtml
      H.title $ case pagetitle of
        Just pagetitle' -> toHtml pagetitle'
        Nothing -> case slug of
          Just slug' -> toHtml slug'
          Nothing -> toHtml $ "Youwen Wu >> " ++ fromMaybe "You aren't supposed to see this." title
      meta ! name "og:site_name" ! content "Youwen’s Website"
      link ! rel "stylesheet" ! href "/css/main.css"
      link ! rel "icon" ! href "/favicon.ico"
      link
        ! rel "preload"
        ! as "font"
        ! href "/fonts/valkyrie_ot_a_regular.woff2"
        ! crossorigin ""
      link
        ! rel "preload"
        ! as "font"
        ! href "/fonts/valkyrie_ot_a_italic.woff2"
        ! crossorigin ""
      H.style inlinedFontCss

desktopSidebar :: Html
desktopSidebar = aside ! class_ "hidden md:block w-64 flex-none" $ do
  a
    ! href "/"
    ! class_ "inline-flex justify-between gap-4 hover:bg-subtle/50 transition-colors mt-3"
    $ do
      preEscapedToHtml logoSvg
      H.span ! class_ "italic text-[2.5em] select-none -translate-y-[6px]" $ "youwen wu"
  nav ! class_ "space-y-4 mt-4" $ do
    ul ! class_ "space-y-2 text-love text-2xl" $ do
      li $ a ! class_ "hover:bg-surface transition-colors" ! href "/about" $ "About"
      li $ a ! class_ "hover:bg-surface transition-colors" ! href "/now" $ "Now"
      li $ a ! class_ "hover:bg-surface transition-colors" ! href "/cv" $ "CV"
    H.div ! class_ "space-y-1" $ do
      p ! class_ "all-smallcaps text-lg" $ "Hacking"
      ul ! class_ "space-y-2 text-subtle text-base" $ do
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "https://functor.systems" $
            "functor.systems"
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "/software/epilogue" $
            "How this site was made"
        li $ a ! class_ "hover:bg-surface transition-colors" ! href "/tools" $ "Favorite tools"
        li
          $ a
            ! class_ "hover:bg-surface transition-colors"
            ! href "/writing/anatomy-of-a-nixos-module"
          $ "Anatomy of a NixOS module"
    H.div ! class_ "space-y-1" $ do
      p ! class_ "all-smallcaps text-lg" $ "Math"
      ul ! class_ "space-y-2 text-subtle text-base" $
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "/math/three-isomorphism-theorems" $
            "Three isomorphism theorems"
    H.div ! class_ "space-y-1" $ do
      p ! class_ "all-smallcaps text-lg" $ "Fun"
      ul ! class_ "space-y-2 text-subtle text-base" $
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "/misc/fav-songs" $
            "Favorite songs"
    H.div ! class_ "space-y-1" $ do
      p ! class_ "all-smallcaps text-lg" $ "Other"
      ul ! class_ "space-y-2 text-subtle text-base" $ do
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "/faqs" $
            "Frequently asked questions"
        li $ a ! class_ "hover:bg-surface transition-colors" ! href "/impressum" $ "Impressum"
        li $
          a ! class_ "hover:bg-surface transition-colors" ! href "/about-this-site" $
            "About this site"
  H.div ! class_ "mt-6" $
    a ! href "/buttons" ! class_ "hover:brightness-75" $
      img ! class_ "w-20" ! src "/static/logo/button.png" ! alt "my button"

mobileHeader :: Html
mobileHeader = header ! class_ "md:hidden border-b border-dashed border-muted mb-8 pb-8 w-full" $ do
  H.div ! class_ "w-full flex justify-center"
    $ a
      ! href "/"
      ! class_
        "inline-flex justify-between gap-4 hover:bg-subtle/50 transition-colors mt-8 mx-auto"
    $ do
      preEscapedToHtml logoSvg
      H.span ! class_ "italic text-[3em] text-center select-none -translate-y-2 mx-auto" $
        "youwen wu"
  details ! class_ "w-full mt-4" $ do
    H.summary ! class_ "text-center smallcaps text-xl cursor-pointer" $ "menu"
    nav ! class_ "space-y-4 text-2xl mt-3" $ do
      ul ! class_ "space-y-3 text-2xl text-love" $ do
        li $ a ! class_ "hover:bg-surface transition-colors" ! href "/about" $ "About"
        li $ a ! class_ "hover:bg-surface transition-colors" ! href "/now" $ "Now"
        li $ a ! class_ "hover:bg-surface transition-colors" ! href "/cv" $ "CV"
      H.div ! class_ "space-y-2" $ do
        H.span ! class_ "all-smallcaps text-lg" $ "Hacking"
        ul ! class_ "space-y-2 text-subtle text-lg" $ do
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "https://functor.systems" $
              "functor.systems"
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "/software/epilogue" $
              "How this site was made"
          li $ a ! class_ "hover:bg-surface transition-colors" ! href "/tools" $ "Favorite tools"
          li
            $ a
              ! class_ "hover:bg-surface transition-colors"
              ! href "/writing/anatomy-of-a-nixos-module"
            $ "Anatomy of a NixOS module"
      H.div ! class_ "space-y-2" $ do
        H.span ! class_ "all-smallcaps text-lg" $ "Math"
        ul ! class_ "space-y-2 text-subtle text-lg" $
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "/math/three-isomorphism-theorems" $
              "Three isomorphism theorems"
      H.div ! class_ "space-y-2" $ do
        H.span ! class_ "all-smallcaps text-lg" $ "Fun"
        ul ! class_ "space-y-2 text-subtle text-lg" $
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "/misc/fav-songs" $
              "Favorite songs"
      H.div ! class_ "space-y-1" $ do
        H.span ! class_ "all-smallcaps text-lg" $ "Other"
        ul ! class_ "space-y-2 text-subtle text-lg" $ do
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "/faqs" $
              "Frequently asked questions"
          li $ a ! class_ "hover:bg-surface transition-colors" ! href "/impressum" $ "Impressum"
          li $
            a ! class_ "hover:bg-surface transition-colors" ! href "/about-this-site" $
              "About this site"

pageFooter :: String -> String -> String -> Html
pageFooter commit ghc time = footer ! class_ "border-t mt-8 border-solid border-muted mb-4 text-sm text-muted py-1" $ do
  p ! class_ "all-smallcaps leading-[1.3]" $ do
    "© 2025 Youwen Wu. Generated by "
    a ! class_ "text-link" ! href "https://github.com/youwen5/web" $ "Hakyll"
    " from "
    a
      ! class_ "text-link"
      ! href (toValue $ "https://github.com/youwen5/web/commit/" ++ commit)
      $ toHtml commit
    " using the Glorious Glasgow Haskell Compiler "
    toHtml ghc
    " at "
    toHtml time
    ". Most content CC-BY-SA-4.0. This page uses "
    a ! href "/privacy" ! class_ "text-link" $ "analytics."
  H.div ! class_ "mt-4 flex flex-wrap not-prose" $ do
    a ! href "https://web.youwen.dev/static/img/anti-js-js-club.png" $
      img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/anti-js-js-club.png"
    img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/htmldream.gif"
    img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/css.gif"
    a ! href "https://neovim.io/" $
      img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/neovim.png"
    a ! href "https://nixos.org/" $
      img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/nixos.png"
    img ! width "88px" ! height "31px" ! alt "" ! src "/static/img/transnow2.gif"
  script ! src "https://unpkg.com/lucide@latest" $ ""
  script "lucide.createIcons();"

giscusComponent :: Html
giscusComponent = do
  H.div ! class_ "smallcaps text-muted w-full text-center mt-6 mb-8 text-3xl" $
    a ! href "/" ! class_ "hover:text-love" $
      "yw"
  script
    ! src "https://giscus.app/client.js"
    ! dataRepo "youwen5/web"
    ! dataRepoId "R_kgDOOc2JBQ"
    ! dataCategory "Announcements"
    ! dataCategoryId "DIC_kwDOOc2JBc4Cp8xj"
    ! dataMapping "pathname"
    ! dataStrict "1"
    ! dataReactionsEnabled "1"
    ! dataEmitMetadata "0"
    ! dataInputPosition "top"
    ! dataTheme "https://web.youwen.dev/styles/giscus.css"
    ! dataLang "en"
    ! dataLoading "lazy"
    ! crossorigin "anonymous"
    ! async ""
    $ ""

defaultTemplate_ :: Bool -> Bool -> Context String -> Item String -> Compiler Html
defaultTemplate_ enableComments wide ctx item =
  do
    title <- getField' "title"
    author <- getField' "author"
    date <- getField' "date"
    pagetitle <- getField' "pagetitle"
    location <- getField' "location"
    slug <- getField' "slug"
    enableComments' <- getField' "enable-comments"
    ghc <- getField' "ghc-version"
    time <- getField' "last-commit-timestamp"
    commitHash <- getField' "commit-hash"
    let ghc' = fromMaybe "GHC_VER_PLACEHOLDER" ghc
    let commitHash' = fromMaybe "COMMIT_HASH_PLACEHOLDER" commitHash
    let time' = fromMaybe "TIME_PLACEHOLDER" time
    return $ docTypeHtml ! lang "en" $ do
      pageHead title pagetitle slug
      body
        ! class_
          "antialiased mt-4 lg:mt-20 leading-relaxed mx-auto max-w-[1200px] flex gap-8 px-4 lg:px-6"
        $ do
          desktopSidebar
          H.div ! class_ "flex-1 md:mt-2" $ do
            mobileHeader
            main
              ! class_
                ( toValue
                    ("main-content" ++ (if not wide then " lg:max-w-[40rem]" else ""))
                )
              $ do
                h1 ! class_ "all-smallcaps md:text-3xl text-2xl text-center mt-4" $ forM_ title toHtml
                H.div ! class_ "space-y-1 text-center mt-4" $ do
                  forM_ date $ \date' -> p ! class_ "text-subtle text-md md:text-lg" $ toHtml date'
                  forM_ location $ \location' -> p ! class_ "text-subtle text-md md:text-lg" $ toHtml location'
                  forM_ author $ \author' -> p ! class_ "text-lg md:text-xl mt-5" $ em "by " >> toHtml author'
                H.div
                  ! class_
                    "prose-lg lg:prose-xl prose-headings:all-smallcaps prose-headings:text-love prose-h1:text-foreground prose-list-snazzy prose-table-snazzy scroll-smooth mt-8"
                  $ preEscapedToHtml (itemBody item)
                when (enableComments || (fromMaybe "false" enableComments' == "true")) giscusComponent
                pageFooter commitHash' ghc' time'
 where
  getField' = getStringField ctx item

defaultTemplate :: Context String -> Item String -> Compiler Html
defaultTemplate = defaultTemplate_ False False

postTemplate :: Context String -> Item String -> Compiler Html
postTemplate = defaultTemplate_ True False

wideTemplate :: Context String -> Item String -> Compiler Html
wideTemplate = defaultTemplate_ False True

-- icon :: String -> Html
-- icon xs = H.span ! class_ "my-auto w-[24px]" $ H.i ! dataLucide xs

postListItem :: Context t -> Item t -> Compiler Html
postListItem ctx item = do
  title <- getField' "title"
  description <- getField' "description"
  url <- getField' "url"
  date <- getField' "date"
  pure
    $ li
    $ a
      ! href (toValue $ fromMaybe "#" url)
      ! class_
        "w-full font-serif block gap-1 py-0 px-2 md:px-1 md:py-1 my-4 md:my-0 border-l-2 border-l-foreground md:border-l-0 hover:bg-foreground hover:text-bg space-y-1"
    $ do
      H.div
        ! class_
          "flex justify-between flex-wrap-reverse content-center gap-x-2 gap-y-1 md:gap-4"
        $ do
          H.span ! class_ "inline-flex gap-3" $ toHtml $ fromMaybe "broken" title
          H.span ! class_ "inline-flex gap-4" $ do
            forM_
              date
              ( (H.span ! class_ "font-light text-lg my-auto")
                  . (H.span ! class_ "all-smallcaps inline-block")
                  . toHtml
              )
      forM_ description $
        (p ! class_ "text-subtle text-sm md:pl-2 md:border-l-2 md:border-subtle") . toHtml
 where
  getField' = getStringField ctx item

-- <span class="inline-flex gap-3"><span class="my-auto w-[24px]"></span>Three Isomorphism Theorems in Linear Algebra</span>

archivePage :: Context String -> Item String -> Compiler Html
archivePage ctx item = do
  let getList' = getList ctx item
  ListData innerCtx posts <- getList' "posts"
  sortedPosts <- recentFirst posts
  postsRendered <- mapM (postListItem innerCtx) sortedPosts
  pure $ do
    p
      "Here are most of the blog style posts on this site, sorted chronologically from newest to oldest. This is not an exhaustive sitemap."
    H.div ! class_ "mx-auto max-w-10 border-t-1 border-t-foreground mb-4" $ ""
    ul ! class_ "not-prose" $ mconcat postsRendered

indexTemplate :: Context String -> Item String -> Compiler Html
indexTemplate ctx item =
  do
    title <- getField' "title"
    pagetitle <- getField' "pagetitle"
    slug <- getField' "slug"
    ghc <- getField' "ghc-version"
    time <- getField' "last-commit-timestamp"
    commitHash <- getField' "commit-hash"
    let ghc' = fromMaybe "GHC_VER_PLACEHOLDER" ghc
    let commitHash' = fromMaybe "COMMIT_HASH_PLACEHOLDER" commitHash
    let time' = fromMaybe "TIME_PLACEHOLDER" time

    return $ docTypeHtml ! lang "en" $ do
      pageHead title pagetitle slug
      body
        ! class_
          "antialiased mt-4 lg:mt-20 leading-relaxed mx-auto max-w-[1200px] flex gap-8 px-4 lg:px-6"
        $ do
          desktopSidebar
          H.div ! class_ "flex-1 md:mt-2" $ do
            mobileHeader
            main ! class_ "main-content" $ do
              H.div
                ! class_
                  "prose-lg lg:prose-xl prose-headings:all-smallcaps prose-headings:text-love prose-h1:text-foreground prose-list-snazzy prose-table-snazzy scroll-smooth mt-2"
                $ preEscapedToHtml (itemBody item)
              pageFooter commitHash' ghc' time'
 where
  getField' = getStringField ctx item

inlinedFontCss :: Html
inlinedFontCss =
  """
  @font-face {
    font-family: "Source Sans 3";
    font-style: normal;
    font-display: swap;
    font-weight: 300;
    src: url("/fonts/source-sans-3-latin-300-normal.woff2") format("woff2");
  }

  @font-face {
    font-family: "Source Sans 3";
    font-style: normal;
    font-display: swap;
    font-weight: 400;
    src: url("/fonts/source-sans-3-latin-400-normal.woff2") format("woff2");
  }

  @font-face {
    font-family: "Valkyrie A";
    font-style: normal;
    font-weight: normal;
    font-stretch: normal;
    font-display: swap;
    src: url("/fonts/valkyrie_ot_a_regular.woff2") format("woff2");
  }

  @font-face {
    font-family: "Valkyrie A";
    font-style: italic;
    font-weight: normal;
    font-stretch: normal;
    font-display: swap;
    src: url("/fonts/valkyrie_ot_a_italic.woff2") format("woff2");
  }

  @font-face {
    font-family: "Valkyrie A";
    font-style: normal;
    font-weight: bold;
    font-stretch: normal;
    font-display: swap;
    src: url("/fonts/valkyrie_ot_a_bold.woff2") format("woff2");
  }

  @font-face {
    font-family: "Valkyrie A";
    font-style: italic;
    font-weight: bold;
    font-stretch: normal;
    font-display: swap;
    src: url("/fonts/valkyrie_ot_a_bold_italic.woff2") format("woff2");
  }

  @font-face {
    font-family: "Concourse Index";
    font-style: normal;
    font-weight: normal;
    font-stretch: normal;
    font-display: swap;
    src: url("/fonts/concourse_index_regular.woff2") format("woff2");
  }
  """

logoSvg :: String
logoSvg =
  """
  <?xml version="1.0" encoding="UTF-8" standalone="no"?>
  <svg
    width="50px"
    height="50px"
    class="my-auto"
    viewBox="0 0 300 300"
    version="1.1"
    id="svg1"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:svg="http://www.w3.org/2000/svg"
  >
    <defs id="defs1"></defs>
    <g id="layer2">
      <rect
        style="
          opacity: 1;
          fill: #b4637a;
          fill-opacity: 1;
          stroke: #b4637a;
          stroke-width: 2.02726;
          stroke-dasharray: none;
          stroke-opacity: 1;
        "
        id="rect3"
        width="114.72939"
        height="110.92105"
        x="120.82121"
        y="-18.898195"
        transform="rotate(45)"
      ></rect>
      <rect
        style="
          fill: #56949f;
          fill-opacity: 1;
          stroke: #56949f;
          stroke-width: 2.02717;
          stroke-dasharray: none;
          stroke-opacity: 0.0392157;
        "
        id="rect3-5"
        width="114.72939"
        height="110.92105"
        x="194.88144"
        y="-90.162048"
        transform="rotate(45)"
      ></rect>
    </g>
  </svg>
  """
