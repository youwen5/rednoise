module Style where

import Data.Text.Lazy as TL
import Clay
import Constants
import qualified Clay.Media as Media
import qualified Clay.FontFace as FF
import qualified Clay.Flexbox as FB
import qualified Clay as Clay.Text

abordage :: Css
abordage = fontFace $ do
  fontFamily ["Abordage"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/degheest/Abordage-Regular.otf" (Just FF.OpenType) ]
  fontWeight normal
  fontStyle normal

latitude :: Css
latitude = fontFace $ do
  fontFamily ["Latitude"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/degheest/Latitude-Regular.otf" (Just FF.OpenType) ]
  fontWeight normal
  fontStyle normal

libertinus :: Css
libertinus = fontFace $ do
  fontFamily ["Libertinus"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/libertinus/LibertinusSerif-Regular.ttf" (Just FF.TrueType) ]
  fontWeight normal
  fontStyle normal

libertinusItalic :: Css
libertinusItalic = fontFace $ do
  fontFamily ["Libertinus Italic"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/libertinus/LibertinusSerif-Italic.ttf" (Just FF.TrueType) ]
  fontWeight normal
  fontStyle normal

lilex :: Css
lilex = fontFace $ do
  fontFamily ["Lilex"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/lilex/Lilex-Regular.otf" (Just FF.OpenType) ]
  fontWeight normal
  fontStyle normal

lilexWeighted :: Css
lilexWeighted = fontFace $ do
  fontFamily ["Lilex Medium"] [ ]
  fontFaceSrc [ FF.FontFaceSrcUrl "../fonts/lilex/Lilex-Medium.otf" (Just FF.OpenType) ]
  fontWeight normal
  fontStyle normal

fonts :: Css
fonts = do
  abordage
  latitude
  libertinus
  libertinusItalic
  lilex
  lilexWeighted

uniformLinkStyle :: Css
uniformLinkStyle = do
  textDecoration none
  color deepblue

gradientAnimation :: Css
gradientAnimation = do
  keyframes "gradient" [
    (0, backgroundPosition (positioned (pct 0) (pct 0))),
    (100, backgroundPosition (positioned (pct 100) (pct 100))) ]

styles :: Css
styles = do
  fonts
  gradientAnimation

  ".image-link" ? uniformLinkStyle

  "::selection" ? do
    backgroundColor deepblue
    color white

  "#bg" ? do
    position fixed
    zIndex (-1)
    top (px 0)
    left (px 0)
    width (vw 100)
    height (vh 100)
    background (linearGradient (angular (deg (180 - 45))) [
      (rgba 255 0 0 0.2, pct 0),
      (rgba 255 64 0 0.2, pct (100 / 12)),
      (rgba 255 128 0 0.2, pct (2 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (3 * 100 / 12)),
      (rgba 255 128 0 0.2, pct (4 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (5 * 100 / 12)),
      (rgba 255 255 0 0.2, pct (6 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (7 * 100 / 12)),
      (rgba 255 0 255 0.2, pct (8 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (9 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (10 * 100 / 12)),
      (rgba 255 255 255 0.2, pct (11 * 100 / 12)),
      (rgba 255 255 255 0.2, pct 100)
      ])
    backgroundSize (by (pct 800) (pct 800))
    animation "gradient" (sec 7) ease (sec 0) (iterationCount 1) normal forwards

  html ? do
    fontSize (pct 62.5)
    fontFamily ["Libertinus"] []

  body ? do
    fontSize (Clay.rem 2)
    color black

  code ? do
    fontFamily ["Lilex Medium"] []
    fontSize (Clay.rem 2.1)
    paddingLeft (Clay.rem 0.5)
    paddingRight (Clay.rem 0.5)

  ".math" ? do
    display inlineBlock

  ".block-math" ? do
    display flex
    justifyContent (JustifyContentValue centerValue)
    minWidth (pct 100)
    width (px 0)
    overflowX auto

  figure ? do
    display block
    justifyContent (JustifyContentValue centerValue)
    minWidth (pct 100)
    width (px 0)
    marginLeft auto
    marginRight auto
    overflowX auto
    marginTop (Clay.rem 2.5)
    marginBottom (Clay.rem 2.5)

  figure <? "*" ? do
    display block
    marginLeft auto
    marginRight auto
    textAlign Clay.Text.center

  figure <? figcaption ? do
    marginTop (Clay.rem 1.5)

  p ? do
    fontFamily ["Libertinus"] []
    fontSize (Clay.rem 2.1)
    maxWidth (Clay.rem 70)
    color black

  a ? do
    color deepblue
    textDecoration none
    ":visited" & color deepblue
    ":hover" & do
      backgroundColor deepblue
      color white
    ":active" & textDecoration underline
    ":focus" & textDecoration underline

  blockquote ? do
    color darkgray
    borderLeft (Clay.rem 0.2) solid gray
    paddingLeft (Clay.rem 1)

    p ? do
      fontFamily ["Libertinus"] []
      fontSize (Clay.rem 2.4)
      maxWidth (Clay.rem 50)

    ".quote" ? do
      textAlign start

      "::selection" & do
        color white
        backgroundColor darkblue

    ".attr" ? do
      fontFamily ["Libertinus Italic"] []
      textAlign end
      color darkgray

      "::selection" & do
        color white
        backgroundColor darkblue

      a ? do
        color darkblue

        ":hover" & do
          backgroundColor darkblue
          color white

        "::selection" & do
          color white
          backgroundColor darkblue

  ".cmd" ? do
    fontFamily ["Lilex"] []
    fontSize (Clay.rem 2.5)

  header ? do
    height (Clay.rem 5)
    borderBottom (Clay.rem 0.2) solid black

  nav ? do
    paddingTop (Clay.rem ((5 - 2.2) / 2))
    textAlign end
    a ? do
      fontFamily ["Latitude"] []
      fontSize (Clay.rem 2.2)
      textTransform lowercase
      color black
      textDecoration none
      ":visited" & color black
      ":focus" & textDecoration none
      ":active" & textDecoration none
      ":hover" & do
        textDecoration none
        backgroundColor black
        color white

  footer ? do
    fontFamily ["Latitude"] []
    marginTop (Clay.rem 3)
    padding (Clay.rem 1.2) (Clay.rem 0) (Clay.rem 1.2) (Clay.rem 0)
    borderTop (Clay.rem 0.2) solid black
    fontSize (Clay.rem 1.4)
    color "#555"

    ".left" ? do
      maxWidth (Clay.rem 35)

  h1 ? do
    fontFamily ["Abordage"] []
    fontSize (Clay.rem 7.5)
    marginBottom (px 0)

  h2 ? do
    fontFamily ["Abordage"] []
    fontSize (Clay.rem 5)
    marginBottom (px 0)

  h3 ? do
    fontFamily ["Abordage"] []
    fontSize (Clay.rem 3)

  h4 ? do
    fontFamily ["Latitude"] []
    fontSize (Clay.rem 3)

  "#main-content" ? do
    display flex
    justifyContent Clay.center
    alignItems Clay.center
    ".subtitle" ? do
      display inlineBlock
      width (pct 75)
      marginLeft (px 0)
      marginRight (pct 5)
      verticalAlign middle

  "#maps" ? do
    display flex
    justifyContent Clay.center
    alignItems Clay.center

  "#map-danville" ? do
    marginRight (px 10)

  ".caption" ? do
    fontFamily ["Latitude"] []
    fontSize (Clay.rem 2.2)

  ".username" ? do
    fontFamily ["Lilex Medium"] []
    fontSize (Clay.rem 2.4)

    ":hover" & do
      ".username-tooltip" ? do
        visibility visible

    a ? do
      uniformLinkStyle
      ":hover" & do
        color white
        textDecoration underline

  ".username-tooltip" ? do
    visibility hidden
    width fitContent
    fontSize (Clay.rem 1.6)
    borderRadius (px 5) (px 5) (px 5) (px 5)
    paddingLeft (px 5)
    paddingRight (px 5)
    backgroundColor black
    color white
    position relative
    left (px 10)
    zIndex 1

  ".subtitle" ? do
    fontFamily ["Libertinus"] []
    fontSize (Clay.rem 2.5)

  article ? do
    ".header" ? do
      fontSize (Clay.rem 1.4)
      fontStyle italic
      color "#555"

  ".logo" ? do
    a ? do
      fontSize (Clay.rem 2.2)
      fontFamily ["Latitude"] []
      marginTop (Clay.rem ((5 - 2.2) / 2))
      marginLeft (px 15)
      textDecoration none
      color black

      ":hover" & do
        backgroundColor black
        color white

  "#icon" ? do
    float floatLeft
    height (Clay.rem 4)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    marginLeft (px 15)
    marginRight (px 15)

  ".left" ? do
    textAlign start

  "#main-image" ? do
    display inlineBlock
    width (pct 20)
    margin (px 0) (px 0) (px 0) (px 0)
    borderRadius (px 20) (px 20) (px 20) (px 20)

  query Media.screen [Media.maxWidth (px 475)] $ do
    body ? do
      fontSize (Clay.rem 1.5)

    h1 ? do
      fontSize (Clay.rem 3)

    h2 ? do
      fontSize (Clay.rem 3)

    h3 ? do
      fontSize (Clay.rem 1.8)

    h4 ? do
      fontSize (Clay.rem 1.8)

    ".logo" ? a ? do
      ":hover" & do
        backgroundColor white
        color deepblue

    ".username" ? do
      fontSize (Clay.rem 1.8)

    p ? do
      fontSize (Clay.rem 1.5)

    ".subtitle" ? do
      width (pct 100)
      fontSize (Clay.rem 1.5)
      marginRight (px 0)

    "#main-content" ? do
      flexWrap FB.wrap
      justifyContent flexStart

      ".subtitle" ? do
        width (pct 100)

    "#main-image" ? do
      width (pct 60)

    "#main-image" ? do
      width (pct 40)
      marginBottom (Clay.rem 3)

    "#maps" ? do
      flexWrap FB.wrap

    "#icon" ? do
      marginLeft (px 15)
      marginRight (px 15)

    "#map-danville" ? do
      marginBottom (px 20)

    code ? do
      fontSize (Clay.rem 1.5)

    nav ? do
      display block
      textAlign start
      paddingBottom (Clay.rem 1.4)
      width (pct 100)
      margin (px 0) (px 0) (px 0) (px 0)
      a ? do
        display inline
        margin (Clay.rem 0) (Clay.rem 0) (Clay.rem 0) (Clay.rem 1.2)

    footer ? do
      textAlign start

    header ? do
      height (Clay.rem (10 + 2 * 1.4))

    blockquote ? do
      fontSize (Clay.rem 1.6)
      marginLeft (px 0)
      marginRight (px 0)

      p ? do
        fontSize (Clay.rem 1.6)

    ".caption" ? do
      fontSize (Clay.rem 1.6)

  query Media.screen [Media.minWidth (px 476), Media.maxWidth (px 600)] $ do
    body ? do
      paddingLeft (px 0)

    header ? do
      width (pct 100)
      height (Clay.rem (10 + 1.4))
      borderBottom (Clay.rem 0.2) solid black
      margin (Clay.rem 3) (Clay.rem 0) (Clay.rem 3) (Clay.rem 0)

    nav ? do
      display block
      textAlign start
      width (pct 100)
      margin (px 0) (px 0) (px 0) (px 0)
      a ? do
        display inline
        margin (Clay.rem 0) (Clay.rem 0) (Clay.rem 0) (Clay.rem 1.2)

    h1 ? do
      fontSize (Clay.rem 5)

    h3 ? do
      fontSize (Clay.rem 2)

    h4 ? do
      fontSize (Clay.rem 2)

    ".logo" ? a ? do
      ":hover" & do
        backgroundColor white
        color deepblue

    ".username" ? do
      fontSize (Clay.rem 2)

    p ? do
      fontSize (Clay.rem 1.8)

    "#main-content" ? do
      flexWrap FB.wrap
      justifyContent flexStart

      ".subtitle" ? do
        fontSize (Clay.rem 2)
        width (pct 100)

    blockquote ? do
      width (pct 100)
      marginLeft (px 0)
      marginRight (px 0)

    "#main-image" ? do
      width (pct 40)
      marginBottom (Clay.rem 3)

    "#maps" ? do
      flexWrap FB.wrap

    "#map-danville" ? do
      marginBottom (px 20)

  query Media.screen [Media.minWidth (px 601), Media.maxWidth (px 1023)] $ do
    body ? do
      width (pct 80)
      margin (px 0) auto (px 0) auto

    header ? do
      width (pct 100)
      height (Clay.rem (10 + 1.4))
      borderBottom (Clay.rem 0.2) solid black
      margin (Clay.rem 3) (Clay.rem 0) (Clay.rem 3) (Clay.rem 0)

    nav ? do
      display block
      textAlign start
      width (pct 100)
      margin (px 0) (px 0) (px 0) (px 0)
      a ? do
        display inline
        margin (Clay.rem 0) (Clay.rem 0) (Clay.rem 0) (Clay.rem 1.2)

    "#icon" ? do
      marginLeft (px 0)
      marginRight (px 0)

    footer ? do
      textAlign end

    ".logo" ? do
      display block
      height (Clay.rem 5)
      margin (px 0) (px 0) (px 0) (px 0)
      textAlign start

      a ? do
        float floatLeft
        fontSize (Clay.rem 2.2)

  query Media.screen [Media.minWidth (px 1024)] $ do
    body ? do
      width (Clay.rem 80)
      margin (px 0) auto (px 0) auto

    header ? do
      borderBottom (Clay.rem 0.2) solid black
      margin (Clay.rem 3) (Clay.rem 0) (Clay.rem 3) (Clay.rem 0)

    nav ? do
      margin (px 0) (px 0) (px 0) (px 0)
      a ? do
        display inline
        margin (Clay.rem 0) (Clay.rem 0) (Clay.rem 0) (Clay.rem 1.2)

    footer ? do
      textAlign end

    "#icon" ? do
      marginLeft (px 0)
      marginRight (px 0)

    ".logo" ? do
      margin (px 0) (px 0) (px 0) (px 0)
      textAlign start

      a ? do
        float floatLeft
        fontSize (Clay.rem 2.2)

css :: String
css = TL.unpack $ renderWith compact [] styles
