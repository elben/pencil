Migration to nix:

- validate everything works, testing the various things

# Development

Pencil uses Nix to build its development environment. We develop against

First, install and set up `nix`:

```bash
# Install nix
curl https://nixos.org/nix/install | sh
```

Pin nixpkgs to version 18.09. The nixpkgs-unstable version (19.03) had problems
building pandoc-2.5:

```
nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
nix-channel --update
```

See that pencil builds:

```bash
nix-build --attr env release.nix
```

We'll need to install a couple of Haskell packages that we use for development.
On my macOS, this is how I would install these packages. If this doesn't work,
try following the instructions found
[here](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure)
to figure out the namespace that your OS uses for packages. You may need to just
replace 'nixpkgs' with 'nixos'.

```bash
nix-env -iA nixpkgs.haskellPackages.cabal-install
nix-env -iA nixpkgs.haskellPackages.doctest
```

Once that's installed, we'll want to go into a `nix-shell`. Whereas `nix-build`
builds all the dependencies and the pencil library itself, when we are
developing pencil we want to be dropped into an environment where everything is
set up as if we are about to build pencil, but not actually build it. To do
this, we need to go into a nix shell:

```bash
nix-shell --attr env release.nix
```

And now, inside the `nix-shell`:

```bash
[nix-shell]$ cabal new-test
[nix-shell]$ doctest src/
```

## Documentation

Generate documentation.

```
[nix-shell]$ cabal new-haddock
```

## Ctags

```bash
nix-env -iA nixpkgs.haskellPackages.hasktags
make tags
```

## Nix Maintenance

```bash
# Figure out which release of nixpkgs we are using.
nix-instantiate --eval --expr 'builtins.readFile <nixpkgs/.version>'
nix-instantiate --eval --expr 'builtins.readFile <nixpkgs/.git-revision>'

# Update the current nix channel
nix-channel --update

# Pinning the current channel to a specific version
# Available channels: https://nixos.org/channels/
nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
```

## Release

Check for newer dependency versions: http://packdeps.haskellers.com/feed?needle=pencil

Make sure it builds, passes tests, and works:

```
[nix-shell]$ cabal configure

# Run tests and generate the example websites (e.g. pencil-example-blog).
# The generated websites can be found in this path:
# ./dist-newstyle/build/x86_64-osx/ghc-8.6.3/pencil-0.1.3/t/pencil-example-blog
[nix-shell]$ cabal new-test

[nix-shell]$ cabal new-haddock

# Generate a source distribution file (.tar.gz).
[nix-shell]$ cabal new-sdist

stack build

# stack test runs tests and also the example websites (e.g.
# pencil-example-blog), generating files in their perspective `examples/out/`
# directories.
stack test

stack haddock
stack sdist
```

Check that tutorials are updated.

Update the CHANGELOG.md.

Update the version number in `pencil.cabal`.

Commit the changes.

```
git commit -m "Release version 0.x.x"
```

Tag the release:

```
git tag v0.1.0
git push --tags
```

Make sure the [Travis build is green](https://travis-ci.org/elben/pencil).

Push to Hackage:

```
stack sdist
stack upload
```

## Testing against other GHC

The Travis CS build below tests against other GHC. We can also test other GHCs
using stack:

```
stack --stack-yaml=stack-ghc-8.2.yaml build
stack --stack-yaml=stack-ghc-8.2.yaml exec ...
```

## Travis CI

[travis-ci.org/elben/pencil](https://travis-ci.org/elben/pencil)

Note that `.travis.yml` was generated using [multi-ghc-travis](https://github.com/haskell-hvr/multi-ghc-travis) this:

```
stack exec runghc ~/code/multi-ghc-travis/make_travis_yml_2.hs pencil.cabal > .travis.yml
```

This build checks to make sure that Pencil can build against other versions of
GHC specified in the `.travis.yml`. We want to make sure that our dependency
bounds are large enough to support multiple versions of GHC.
