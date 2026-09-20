# Development

Pencil is a normal Cabal project. We develop and test against GHC 9.6.

## Setup

The easiest way to install a Haskell toolchain is [GHCup](https://www.haskell.org/ghcup/).
It installs GHC and Cabal into `~/.ghcup` and does not require root.

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then install the versions we target and put them on your `PATH`:

```bash
ghcup install ghc 9.6.7 --set
ghcup install cabal recommended --set
```

## Building and testing

```bash
cabal update
cabal build all
cabal test
```

`cabal test` builds and runs every test suite, including the doctests in
`test/Spec.hs` and the examples (which generate websites under
`examples/**/out/`).

To run the full doctests over all of `src`, install `doctest` and use Cabal's
doctest support:

```bash
cabal install doctest --ignore-project
make doctest
# or:
cabal repl --with-compiler=doctest --repl-options='-w -Wdefault'
```

Other useful commands:

```bash
# Build the Haddock documentation.
cabal haddock

# Check that the package is well-formed and generate a source tarball.
cabal check
cabal sdist

# Build and view the Pencil docs site.
cabal test pencil-docs
cd docs/ && python -m http.server 8000
open localhost:8000
```

`make` targets are thin wrappers around these (`make`, `make test`,
`make docs`, `make example-blog`, ...).

Note that the URLs in the generated docs will be broken when viewing locally.
This is because GitHub Pages deploys it to elbenshira.com/pencil, so links
reference `/pencil`.

## Dependencies

Pencil depends on [hsass](https://hackage.haskell.org/package/hsass), which
binds to [libsass](https://github.com/sass/libsass). By default `hlibsass`
compiles its bundled copy of libsass, so no system `libsass` is required — only
a C++ compiler (`g++`) and `make`. If you have libsass installed system-wide,
you can link against it instead:

```bash
cabal build -fhlibsass/externalLibsass
```

## Release

Check for newer dependency versions: http://packdeps.haskellers.com/feed?needle=pencil

Make sure it builds, check for warnings, passes tests, and works:

```bash
cabal build all --ghc-options="-fforce-recomp -Wall -fno-code"
cabal test
```

Update the CHANGELOG.md and the version number in `pencil.cabal`, then commit.

Tag the release and push to Hackage:

```bash
git tag v0.x.x
git push --tags

cabal check
cabal sdist
cabal upload path/to/pencil-x.x.x.tar.gz
cabal upload --publish path/to/pencil-x.x.x.tar.gz
```
