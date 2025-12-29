---
title: Colophon
---

How this site was made.

## Typography

A lot of thought has gone into the choice of typefaces used in the design of this website.
Every font has a [history](https://letterformarchive.org/news/a-toolkit-for-learning-type-history/), so I thought I'd dedicate a section to explaining the stories behind each typeface I selected for the site.

First, though, I'd like to credit the designers of the typefaces and list, for each typeface, where I use it in the site:

- Headers use [**Abordage**](https://velvetyne.fr/degheest/abordage.html)
- Subtitles and other distinctive text use [**Latitude**](https://velvetyne.fr/degheest/latitude.html)
- Main body text uses [**Libertinus Serif**](https://github.com/alerque/libertinus)
  - Sourced from [Google Fonts](https://github.com/googlefonts/libertinus)
- Code uses [**Lilex**](https://lilex.myrt.co/)
- Inline and block equations use some variant of [**Computer Modern**](https://en.wikipedia.org/wiki/Computer_Modern)
  - The exact font details depend on [KaTeX](https://katex.org/)'s rendering process

All fonts used in this site are licensed under version 1.1 of the [**SIL Open Font License**](https://openfontlicense.org/), meaning they are free (as in speech *and* as in beer) and open-source.

Both Abordage and Latitude are part of the [**Degheest**](https://velvetyne.fr/degheest/) collection, described below.

### Degheest

The Degheest collection is a set of typefaces designed by [Ange Degheest](https://velvetyne.fr/degheest/) (1928--2009) and digitized by the independent libre French type foundry [Velvetyne](https://velvetyne.fr/).
It consists of six distinct families, two of which are used in this site: Abordage and Latitude.

Degheest designed fonts for the [Minitel](https://en.wikipedia.org/wiki/Minitel), the French version of the internet that predated the World Wide Web.
In a sense, the revival of her work is a journey through the depths of time, back to the origins of the internet itself.
The collection is stunning in its beauty and versatility, but Ange Degheest herself has largely been forgotten along with the [numerous women](https://www.women-in-type.com/) who played pivotal roles throughout the history of typographic design.

The [_Reviving Ange Degheest_ pamphlet](https://www.poem-editions.com/library/degheest) is a powerful collection of poems that speaks to the history of her work and its importance in modern typography.
There is also an [exhibition](https://velvetyne.fr/degheest/infos.html) dedicated to her work organized by the Velvetyne foundry.
You should absolutely explore these resources---fascinating in their own right---but I'll also present a few snippets of her work that I received in the font bundle.

!["Ange Degheest" rendered in each of the fonts in her collection; the fonts on the blue and yellow backgrounds are Abordage and Latitude, respectively](/images/degheest.gif){width=50%}

The source for these fonts is freely available on [GitLab](https://gitlab.com/Eugenie-B/degheest-types/-/tree/master).

#### Abordage

Abordage is a typeface of exploration; it was born from Degheest's study of maritime cartography.
It was specifically modeled after a set of fonts used to label mountains and isolated rocks.
It's perfect for large titles and headings, where the curves and quirky letterforms are visible.
I find it fitting that the first thing you see when opening the site is the beauty of Abordage---it's as if this entire website is merely an island in some faraway ocean.
The adventurous spirit of Abordage is rendered by playful curvature and bold strokes.

![Abordage in its bold, comic form](/images/abordage.png){width=50%}

#### Latitude

Every sans-serif typeface needs a serif counterpart.
It's only fitting that Degheest's signature sans-serif Abordage is balanced by another one of the fonts in her collection.
Latitude is formal but unserious, with a touch of whimsy.
It's letterforms are straight and angular, but bend unconventionally.
Rather than impose an authoritarian order à la Times New Roman etc, Latitude embraces the whimsy of Abordage in serif form.
It provides just the right amount of structure to be legible at smaller sizes but maintains an air of anti-establishment egalitarianism that helps it stand out from ordinary prose.

![The elegance and quirkiness of Latitude on full display](/images/latitude.png){width=50%}

### Libertinus Serif

Libertinus Serif is the default font for prose used by [Typst](https://typst.app/).
The decision to use Libertinus Serif over TeX's standard Computer Modern for standard prose is both pragmatic and stylistic.
On the pragmatic side, Libertinus is much more readable, especially at smaller sizes.
Computer Modern was designed for print, not for on-screen viewing, and hence suffers from [poor legibility](https://www.jaburjak.cz/posts/readable-computer-modern/).

The typeface is essentially a slight variation on the [Linux Libertine](https://en.wikipedia.org/wiki/Linux_Libertine) family.
It is a [transitional serif](https://en.wikipedia.org/wiki/Serif#Transitional) and replaces the proprietary Times New Roman with GNU/Linux-certified typography.
It does not have an authoritarian feel, but instead a more relaxed and modern appearance.

![Did you know Linux Libertine is the font used in the Wikipedia logo?](/images/wikipedia.png){width=50%}

The version control source for this original family is available on [GitHub](https://github.com/libertine-fonts/libertine).

### Lilex

Lilex is *the* font for code, period.
Full stop.
It's the default font on the [Zed](https://github.com/zed-industries/zed-fonts) text editor and essentially [IBM Plex Mono](https://www.ibm.com/plex/) with superpowers.
It was designed and open-sourced by [Mikhael Khrustik](https://github.com/mishamyrt) in the city of [Armavir, Russia](https://www.openstreetmap.org/search?query=Armavir%2C%20Russia#map=4/49.21/39.51&layers=T).
I'm generally not a fan of corporate typefaces, but Lilex blends the interface between human and machine visible in IBM's Plex Mono with the necessary versatility of a font for developers.
Striking that balance is no easy feat, but Lilex perfectly does both---and that powerful combination makes it ideal for displaying code everywhere, including on the web.

The relevant source is on [GitHub](https://github.com/mishamyrt/Lilex).

![Different weights of Lilex](/images/lilex.png){width=50%}

It's so fun to use that I'll demo a code block in Haskell right here:

```haskell

data Wrap = Wrap [String]
data Burrito a = Burrito Wrap a

instance Functor Burrito where
  fmap :: (a -> b) -> (Burrito a) -> (Burrito b)
  fmap f = \(Burrito wrap ingredients) ->
             Burrito wrap (f ingredients)

instance Applicative Burrito where
  pure :: a -> Burrito a
  pure base = Burrito (Wrap []) base
  
  (<*>) :: Burrito (a -> b) -> Burrito a -> Burrito b
  burritoFactory <*> incomingBurrito =
    join $ fmap
      (\burritoMaker -> fmap burritoMaker incomingBurrito)
      burritoFactory

instance Monad Burrito where
  (>>=) :: Burrito a -> (a -> Burrito b) -> Burrito b  
  incomingBurrito >>= burritoMaker =
    (join . fmap burritoMaker) incomingBurrito
 
join :: Burrito (Burrito a) -> Burrito a
join (Burrito (Wrap outer)
     (Burrito (Wrap inner) ingredients)) =
       Burrito (Wrap (outer ++ inner)) ingredients

(>==) :: (a -> Burrito b)
         -> (b -> Burrito c)
         -> (a -> Burrito c)
burritoProducer >== burritoConsumer =
  \ingredients -> burritoProducer ingredients
                  >>= burritoConsumer
  
beanBurrito :: [String] -> Burrito [String]
beanBurrito ingredients =
  Burrito (Wrap ["tortilla"]) (ingredients ++ ["beans"])

compositeBurrito :: Burrito [String]
compositeBurrito = (return >== beanBurrito) ["salsa"]

```

Do you see that monadic bind (`>==`)?
There are some truly beautiful ligatures for the Haskell programmer.
By the way, in case you're wondering, the result is a `Burrito (Wrap ["tortilla"]) ["salsa", "beans"]` (that's a tortilla-wrapped burrito with salsa and beans).
Ok, enough distractions.

### Computer Modern

The quintessential typeface for mathematics.
I don't use it for prose, opting for Libertinus Serif instead, but few fonts can match its elegance when it comes to mathematical symbols in all their glory.
I thus opted for the conventional and elegant, though archaic, Computer Modern for this purpose.
It was created by [Donald Knuth](https://en.wikipedia.org/wiki/Donald_Knuth) and served as the basis for TeX and later LaTeX's mathematical typesetting systems.
Knuth, one of the most famous theoretical computer scientists of the century, is also the author of the famous [_Art of Computer Programming_ book](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming) and no stranger to the art of typesetting.

You've probably seen variants of this font in practically every technical document ever published in LaTeX or similar typesetting programs, but here's some math that's fun to write out:

\\[ 
  \\int_{-\\infty}^{\\infty} e^{-x ^ 2} dx = \\sqrt{\\frac{\\tau}{2}}
\\]

The font is not being actively developed anymore, but you can download the official [TeX package](https://tug.org/FontCatalogue/computermodern/) to inspect the source.

## Financial Disclosures

Every site on the internet that's listed under a human-readable domain name is subject to [some form of a land value tax](https://en.wikipedia.org/wiki/Domain_name_registrar#Commercialization), often paid to a domain registrar per annum.
While I support such taxes in principle as a means of preventing robbery from the public commons and achieving [equitable redistribution](https://en.wikipedia.org/wiki/Georgism), I refuse to be subject to rent extraction by a faceless domain czar.
Thus, I don't pay this tax at all; this site is generously funded by [functor.systems](https://functor.systems) affiliate [`kaitotlex`](https://web.kaitotlex.systems/).
Thanks to his heroic efforts in securing this legendary domain, I have been provided real estate rent-free from the public commons, allowing me to devote my time to the pursuit of knowledge and public intellectualism in its purest form.
