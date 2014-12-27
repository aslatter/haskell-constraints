To build all of the utilities in this folder use:

```bash
cabal sandbox init
cabal install --dependencies-only
cabal build
```

BuildDebs
=======

This program scrapes information from the Debian web-site to build
a `cabal.config` file for a specific Debain version.

Example usage:

```bash
dist/build/builddebs/builddebs sid > sid.cabal.config
```

Progress information will be printed to standard error.

