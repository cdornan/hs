# hs: a Haskell installation-manager broker

Build managers like `cabal-install` and `stack` need access to installed toolchains managed by
installation managers like `stack` and `ghcup`. `hs` keeps track of the toolchains installed by
the managers and locates them on request for the build managers.

```bash
$ hs whereis ghc-8.10.4
/Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4
```

We have a prototype integration for `stack` (watch this space) and a collection of wrappers for
each tool that `cabal-install` can use, like the following for ghc-pkg-8.10.4:

```bash
#!/usr/bin/env bash
hs run ghc-pkg-8.10.4 --ask-install -- "$@"
```

Used in this way, `hs` will try to locate `ghc-pkg-8.10.4`, asking the user if to proceed with an
installation with the preferred installation manager (`stack` or `ghcup`) if it cannot locate the
installation.

To list the installation in your development environment try this:

```
$ stack list
8.6.5      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.6.5
8.8.3      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.8.3
8.8.4      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.8.4
8.10.4     stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4
8.10.5     ghcup /Users/chris/.ghcup/ghc/8.10.5
9.0.1      ghcup /Users/chris/.ghcup/ghc/9.0.1
```

To configure the defaults you can do these.

```bash
# ghcup to carry out installations requested from the build tools
$ hs use ghcup stack
# ask before installing any missing toolchains
$ hs use-install-mode ask-install
# specify the deafult toolchain when no version specified
$ hs use-compiler 8.10.4
```

To dump a new set of wrappers into `~/.hs/bin` with your preferred `ask-install` installation
option:

```bash
$ hs dump-ghc-wrappers ask-install
```

With the path set up right your stack and cabal-install projects should play nicely together.

Share and Enjoy!


## Installation Instructions

  1. Build and install `hs` on your path.

  2. Build and install this
     [patched version of `stack-2.8.0`](https://github.com/cdornan/stack-hs/pull/1)
     and put it on the path.

  3. Put [these wrapper scripts](wrappers) on your path.
