<!-- markdownlint-disable MD013 -->

# Monadic

My new website and blog at [monadi.cc](https://monadi.cc)

---

*Instructions for using [this template](https://code.functor.systems/functor.systems/hask)*

Nix tooling for modern Haskell

---

Getting Haskell to work with Nix is a [notoriously complex enterprise](https://www.reddit.com/r/haskell/comments/1cqfboq/latest_guidance_on_using_haskell_with_nix/).
There are two [package](https://www.haskell.org/cabal/) [managers](https://docs.haskellstack.org/en/stable/), a [package repository](https://hackage.haskell.org/) that is only partially mirrored to [nixpkgs](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=haskellPackages), and at least [five different ways](https://nixos.wiki/wiki/Haskell) to build Haskell projects with Nix. It's so complicated that the Nix team had to create a [flowchart](https://nixos.wiki/wiki/File:Haskell_choice.png) to help people make sense of their options.

Worst of all, none of these options work properly, if at all.
There are restrictions on what versions of GHC you can use, which versions of Cabal or Stack you can use, which versions of GHC you can use with the version of Cabal or Stack you've chosen, and then the standard restrictions on the versions of the Hackage packages you want to install, so they don't conflict with each other (alas, Cabal and Stack are not Nix-pilled reproducible build systems).
Essentially, all Nix builds of Haskell projects are just reproducible wrappers around fundamentally broken and irreproducible Haskell build tools.

While some reliance on Cabal or Stack is unavoidable, we can replace much of this process with purified Nix derivations thanks to the [Haskell.nix](https://github.com/input-output-hk/haskell.nix) project, which we use in this project.
This repository builds on [srid](https://github.com/srid)'s [haskell-template](https://github.com/srid/haskell-template/) to create a reproducible template for building Haskell projects with Nix that works out-of-the-box with the newest technologies.

We don't compromise or settle for old software—this repository relies on bleeding-edge tools, starting with GHC 9.12.2 and Cabal 3.14.
We've made some significant departures from the standard template, such as removing [Relude](https://github.com/kowainik/relude) in favor of [LiquidHaskell](https://ucsd-progsys.github.io/liquidhaskell/) based verification, which makes the safety guarantees of Relude unnecessary.

## Logistics

The default template is designed to work out-of-the-box for `x86_64-linux`.
For other systems, change the `supportedSystems` field in [`flake.nix`](./flake.nix).

Most details about your project can be changed from the [cabal file](./monadic.cabal).
You should rename this file to match the name of your package.
The standard template expects your main program to live in [`src/Main.hs`](./src/Main.hs) with modules [`Constants.hs`](./src/Constants.hs) and [`Utils.hs`](./src/Utils.hs).
You can add additional modules as needed to `other-modules` in the [cabal file](./monadic.cabal).
Any necessary Haskell packages can be added to `build-depends` in the [cabal file](./monadic.cabal).

To run your program as an executable, use `nix run .#monadic:exe:monadic`.

[Just](https://github.com/casey/just) comes configured as a task runner for this repository, with available tasks `docs`, `repl`, and `run`. The first of these will start a local [Hoogle](https://hoogle.haskell.org/) server with access to project-specific modules. The second will start a GHCi REPL with all project-specific modules loaded. The third will run your program as an executable. The last is equivalent to `nix run .#monadic:exe:monadic`.

## Pre-Commit Checks

This repository enables a suite of relevant pre-commit checks that are integrated into the Nix build process (run `nix flake check`).
These checks will also automatically run prior to commits and push requests.
See [cachix/git-hooks.nix](https://github.com/cachix/git-hooks.nix/) for more information about how this works.

To enter a devshell for pre-commit checks, use `nix develop .#runner`.
Then, run `pre-commit run -a` to trigger the standard pre-commit check suite.
Some of these checks will make automatic changes to your codebase, such as formatting and some autofixes for common errors.
This occurs when you run `nix flake check` as well.

## Liquid Haskell

This repository is designed for Haskell code to be formally verified using [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell/).
Liquid Haskell uses SMT solvers like `z3` to verify properties of Haskell code through type refinements.
To learn more about how this works and take advantage of Liquid Haskell refinements, see the [standard tutorial](https://ucsd-progsys.github.io/liquidhaskell-tutorial/).

Liquid Haskell seems impossible to get working with Nix (see [this issue](https://github.com/ucsd-progsys/liquidhaskell/issues/1099) e.g.), since it is a plugin for GHC but also requires external dependencies like `z3`.
Hence, we can only use it in a devshell through GHC itself, and not through [Haskell.nix](https://github.com/input-output-hk/haskell.nix/issues/2433).
As a result, Liquid Haskell support is slightly jank and not fully reproducible like the rest of this repository.

In the main devshell (`nix develop`), use `install-liquid` to install the Liquid Haskell plugin and its dependencies.
This only needs to be done once.
You may need to change `buildInputs` in the `devShells.default` section of [`flake.nix`](./flake.nix) and rerun this command if Liquid Haskell or GHC complain about missing dependencies.

Running `liquid` will run the default Liquid Haskell compile-time type checks.
It expects your entry point to be [`Main.hs`](./src/Main.hs) in [`src`](./src), but this can be changed in the `main` and `dir` variables in the `devShells.default` section of [`flake.nix`](./flake.nix).
You should change the `flags` variable to include any additional language extensions that are needed for the code to compile.
You can automatically run `liquid` whenever the source Haskell files change with `watch-liquid`.

Liquid Haskell will automatically generate an HTML document for each Haskell file with type annotations, which you can view by opening the files in the [`src/.liquid`](./src/.liquid) directory.
It might be useful to launch a [live server](https://github.com/lomirus/live-server) or something similar from this directory.
