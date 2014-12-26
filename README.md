haskell-constraints
============

This project contains `cabal.config` files which may be placed
into the root of a Cabal project's folder to restrict the versions
of packages that the Cabal dependency solver can use to
construct a build, similar to what the `cabal freeze` command
does.

Currently we provide `cabal.config` constraint-files for various
recent releases of the [Haskell Platform][1].

The hope is that you can use sandboxes and convinient build
tools like `cabal install` to build your package and transparently
pull in dependencies while still testing against a realistic
environment that some of your consumers may try to use.

I'm not sure yet if this is a good idea, and it is probably more
useful to library developers than application developers (an
application developer can just use `cabal freeze` after all). I'm
putting this out here because when I wanted to see if this would
be helpful I didn't see anything like this already published - other
than the `cabal.config` files [published for Stackage builds][2].

[1] https://www.haskell.org/platform/changelog.html
[2] http://www.stackage.org/lts/cabal.config
