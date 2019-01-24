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

```bash
nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
nix-channel --update
```

See that pencil builds:

```bash
nix-build --attr env default.nix
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
nix-shell --attr env
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

## IDE

Getting various IDEs set up to develop Pencil.

First, install [hie-nix](https://github.com/domenkozar/hie-nix). Follow the instructions in the repo, but beware that installing all versions of hie for all versions of GHC takes a LONG time on macOS (because it needs to compile them all). I just install the specific version of hie-nix/ghc I need.

There is a script in Pencil's directory, `hie-start.sh`, that properly runs `hie-wrapper` in the context of our `nix-shell`. We're going to use this script to run the Haskell Language Server.

### Visual Studio Code

Install the [vscode-hie-server](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server) extension.

In the workspace settings, turn on **Language Server Haskell: Use Custom Hie Wrapper**.
Then set **Language Server Haskell: Use Custom Hie Wrapper Path** to `${workspaceFolder}/hie-start.sh`.
This way, we use the `hie-start.sh` script to properly start HIE in a nix-shell.

The JSON setting would look like:

```json
{
    "languageServerHaskell.useCustomHieWrapper": true,
    "languageServerHaskell.useCustomHieWrapperPath": "${workspaceFolder}/hie-start.sh"
}
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
[nix-shell]$ cabal new-configure

# Run tests and generate the example websites (e.g. pencil-example-blog).
# Check the ./examples/**/out/ directories for the generated websites.
[nix-shell]$ cabal new-test && doctest src/
[nix-shell]$ cabal new-run pencil-example-simple
[nix-shell]$ cabal new-run pencil-example-blog

[nix-shell]$ cabal new-haddock

# Generate a source distribution file (.tar.gz).
[nix-shell]$ cabal new-sdist
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

Make sure the [Circle build is green](https://circleci.com/gh/elben/pencil).

Push to Hackage:

```
cabal new-sdist
cabal upload
```

## Testing against other GHC

CircleCI build also checks against other versions of GHC. To manually test it, just specify the correct .nix file:

```
nix-shell --attr env release-ghc-8.6.nix
```
