# Towards a Common Haskell Development Environment

One of [the projects](https://haskell.foundation/projects/) we have been pursuing at the Haskell
foundation is a unified installer. Hitherto things have been a little fragmented when it comes to
Haskell installers, not covering Windows very uniformly and, of course, requiring different
installers depending upon whether you want to work with `stack` or `cabal-install`. (I distinguish
between the [`Cabal`](https://hackage.haskell.org/package/Cabal) API used by both
[`stack`](https://hackage.haskell.org/package/stack) and the
[`cabal-install`](https://hackage.haskell.org/package/cabal-install) application, using the package
names to keep everything straight.)

This is really unfortunate for many reason.

  * Poor Windows coverage restricts access to Haskell, with a viscous whereby the poorer the
    Windows tooling, the shallower the pool of Windows Haskell developers to improve or even
    maintain it.

  * It reinforces a tendency to silo developers with those using Posix/Windows or
    `stack`/`cabal-install`, contributing to a growing incomprehension of each other's
    problems, even for those that actively want to see more uniformity in development
    environments. It is difficult to overstate the problems this causes.

  * Even for those that are prepared to build their own unified development environment it is not
    currently practical to do so based on the commonly used tools &mdash; unless you are prepared to
    patch the tools and/or install dozens upon dozens of wrappers (see below) as the current tools
    will install toolchains without regard those that are currently installed. It is of course
    easy enough to install one along side the other for a specific use but they will manage
    their own duplicate toolchains without constant intervention.

  * Writing satisfactory Haskell introductory tutorials is nearly impossible to do. You have to
    either pick one of the build systems and cover that properly, most likely on Posix, or try to
    to cover everything in detail which will immediately restrict your audience, or abstract away
    from the details and your audience will get culled when it comes to trying things out.

This list isn't comprehensive. You can probably add to it.


## How to fix?

Clearly we want all Haskell installers like `stack` and `ghcup` to install both `cabal-install` and
`stack` in such a way that they will use a toolchain that either installs, and ideally allow the
developer to control which tool-chain installer gets invoked regardless of whether you are building
with `stack` or `cabal-install` from a clean checkout. (Currently only `stack` will auto-install
a missing toolchain but there is no reason why could not trigger an installation with the right
hooks.)

Cross-installing the build tools is almost trivial (`ghcup` is already unofficially doing it) &mdash;
getting them to manage the toolchains coherently thereafter is the hard part.

I proposed a practical solution &mdash; `stack` and `ghcup` each use entirely straightforward and
stable organisations of their toolchains. Before each is about to install another toolchain they
could check whether the other had already installed it an link it in rather than downloading and
installing another GHC bindist. Perhaps predictably, nobody was keen on this solution, as they
would be relying on undocumented interfaces going forward, clearly risking problems down the line.

While initially disappointed I have come to see this as correct. It also opens up the opportunity to
allow users full control of which system manages the toolchain installations.


## `hs`: a tooolchain installation broker

Whenever `cabal-install` or `stack` wish to locate a toolchain (say, `ghc-8.10.4`), and they have
been configured to do so, they will call out to a new `hs` broker service:

```bash
$ hs whereis ghc-8.10.4
/Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4
```

(Here we are `exec`ing hs, but an internal `hs` library call is also available.)

`hs` responds with the location of the root of the installed bindist if it is already installed (in
this case clearly a stack managed toolchain).

If the toolchain is not installed then, depending upon how `hs` is configured, it will yield an
error indicating that no such toolchain could be located, or it will auto-download-and-install
the toolchain with the designated default toolchain installer, or it will ask the developer
if they would like to download and install the required GHC bindist.

The build manager can specify this behaviour explicitly.

```bash
$ hs whereis --install     ghc-8.10.4
$ hs whereis --no-install  ghc-8.10.4
$ hs whereis --ask-install ghc-8.10.4
```


## `hs`: a PATH interface for cabal-install

Traditionally, cabal install uses the `PATH` to resolve the location of the tools. If `ghc-pkg` for
GHC 8.10.4 is required then `ghc-pkg-8.10.4` will be `exec`ed. Using this we can install a
bunch of wrapper scripts on the `PATH` containing the following:

```bash
#!/usr/bin/env bash
hs run ghc-pkg-8.10.4 -- "$@"
```

Instead of yielding the location of the _bindist_ `hs run ghc-pkg-8.10.4` run the command directly
passing through all of the arguments after the `--` without further interpretation. The wrapper
scripts can rely on the configured installation behaviour or specify it explicitly.

Of course, for this to work `~/.hs/bin` must be on the path ahead of anything else that might have
`ghc-pkg-8.10.4` and friends.


# configuring `hs` and listing installations

The installation manager priority, default install mode and default toolchain can configured or
printed (by specifying no argument) as follows:

```bash
# ghcup to carry out installations requested from the build tools
$ hs use ghcup stack
# ask before installing any missing toolchains
$ hs use-install-mode ask-install
# specify the deafult toolchain when no version specified
$ hs use-compiler 8.10.4
```

To list the installation in your development environment:

```
$ stack list
8.6.5      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.6.5
8.8.3      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.8.3
8.8.4      stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.8.4
8.10.4     stack /Users/chris/.stack/programs/x86_64-osx/ghc-8.10.4
8.10.5     ghcup /Users/chris/.ghcup/ghc/8.10.5
9.0.1      ghcup /Users/chris/.ghcup/ghc/9.0.1
```


# configuring the build tools

Currently `stack` will use its internal GHC installations while `cabal-install` will rely on the
path. It would be great if their global configurations would allow the user or bootstrap Haskell
installer to configure them to defer to specified broker as follows:

```yaml
installer-broker: hs
```

This would be bigger departure for `cabal-install` but it should be able to quite easily make call
out to the broker on parsing the project file and discovering a need for sandboxed toolchain, at
which point it could internally put the toolchain returned by the broker on its path, thereby -- in
this new broker mode -- relieving the developer of the need to manage the `PATH` to capture the
relevant toolchain.

Both build systems would have converged on the same behaviour when it comes to toolchain management.


# a working prototype

I have `hs` written, a patch for `stack` and wrappers for `cabal-install`, yielding a unified system
in which all of the toolchains are made available to both systems as installations are demanded and
added by `stack` or `ghcup` according to the `hs` configuration.
