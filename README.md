# rednoise

This repository is an experiment with rewriting my personal website using
Hakyll. Originally, I had written my own static site generator in Rust, called
Epilogue, with the express goal of using Typst's HTML export to generate all
the pages.

9 months later, at the [functor.systems winter
hackathon](https://code.functor.systems/functor.systems/-/projects/3), we
figured out how to write a Typst compiler for the
[Hakyll](https://jaspervdj.be/hakyll/) system, which is a static site
generator _framework_ written in Haskell, wherein you specify build rules in a
Haskell eDSL. This allowed the integration of the Typst rendering from Epilogue
into a fully-featured static site generation infrastructure. This mostly
obviated the need for my own SSG system and I began looking into porting my
whole website to Hakyll and Haskell. I began by forking
[q9i/monadic](https://code.functor.systems/q9i/monadic), which was where we
originally developed the Typst-Hakyll integration, and the `liquidhaskell`
integration, but it has since diverged quite a bit, and, dare I say, become a
bit more advanced than even `monadi.cc`.

After a few days of hacking, I have mostly succeeded, porting my website almost
1:1 to Hakyll. This has allowed me to leverage the features of Hakyll to
trivially build blog features like RSS feeds and automatic feed generation,
amongst other things, which were nontrivial features that would have required
significant development effort in Epilogue that I had been putting off.

Along the way, I also replaced the Hakyll HTML templating system with
[blaze-html](https://hackage.haskell.org/package/blaze-html), an HTML
combinator library for Haskell. The motivation for this was inspired by my use
of the [hypertext](https://crates.io/crates/hypertext) crate in Epilogue
rather than a traditional HTML templating system, which implemented HTML
templating as a Rust macro. I really liked the fact that I could program the
HTML template in the exact same language the rest of the project was written
in, so I integrated `blaze-html` into Hakyll, replacing the usual HTML
templates. Due to the power and expressiveness of Haskell, `blaze-html` is
simply implemented as a set of combinators in Haskell itself, without
metaprogramming (more specifically, as a monad). The CSS is also slated to be
written in the CSS monad, from
[clay](https://hackage.haskell.org/package/clay), a CSS preprocessor which is
implemented as a Haskell eDSL similar to `blaze-html`.

And all of this Haskell development has been done with the help of
[LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell/) (LH), which
_refines_ Haskell's types with logical predicates to provide guarantees about
the code. In some sense, the behavior of this website has been checked by a
theorem prover.

Finally, I also developed a system for piping data from Hakyll _back_ into
Typst, which allows the Typst document to dynamically generate data based on
the context passed in from Hakyll, for instance, to create an automatically
generated list of recent posts on the homepage.

Once the final finishing touches are placed, the entirety of
[youwen5/web](github.com/youwen5/web) will be replaced by the contents of this
repository, retiring Epilogue for good.

Once again, all of these efforts were done within the 2-week long
[functor.systems winter 2025
hackathon](https://code.functor.systems/functor.systems/-/projects/3).
