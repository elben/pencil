# Development

Pencil uses Nix to build its development environment. We develop against

First, install and set up `nix`:

```bash
# Install nix
curl https://nixos.org/nix/install | sh
```

Pin nixpkgs to version 19.03:

```bash
nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
nix-channel --update
```

See that pencil builds:

```bash
nix-build --attr env
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

We need to specify the `env` attribute for Haskell development, so that
`nix-shell` knows to load the `env` attribute from the final derivation, which
holds all the Haskell goodies that we need. See
[this](https://github.com/Gabriel439/haskell-nix/blob/9c72b6ecbc5e25df509dfd6ee3d5ee8b9eb21f14/project0/README.md#building-with-cabal)
for more info.

And now, inside the `nix-shell`:

```bash
[nix-shell]$ cabal test
[nix-shell]$ doctest src/
```

If there are problems with missing `zlib` dependencies, this is a [known GHC
bug #11042](https://gitlab.haskell.org/ghc/ghc/issues/11042) (I think). The
problem is that our environment inside `nix-shell` has `zlib` in the nix store
instead of the default location. We can try to resolve them by making sure we
load the `nix-shell` with the `zlib` package:

```
nix-shell --attr env -p zlib
```

## IDE

Getting various IDEs set up to develop Pencil.

### Using GHCid and Visual Studio Code

https://github.com/ndmitchell/ghcid

```
nix-env -iA nixpkgs.haskellPackages.ghcid
```

Install the VS Code plugin
https://github.com/ndmitchell/ghcid/tree/master/plugins/vscode

In the Workspace Settings, under **Ghcid: Command**, set the command to:

```
./ghcid-start.sh
```

This is a local script that starts `ghcid` in a Nix shell with the correct
configs.

Now run the **Start Ghcid** VS Code command.

### Using HIE and Visual Studio Code

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

### Ctags

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
nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
```

## Release

Check for newer dependency versions: http://packdeps.haskellers.com/feed?needle=pencil

Make sure it builds, passes tests, and works:

```
[nix-shell]$ cabal configure

[nix-shell]$ doctest src/

# Run tests and generate the example websites (e.g. pencil-example-blog).
# Check the ./examples/**/out/ directories for the generated websites.
[nix-shell]$ cabal test
```

```
# Build documentation
[nix-shell]$ cabal haddock

...
Documentation created: dist/doc/html/pencil/index.html
...
```

```
# Generate a source distribution file (.tar.gz).
[nix-shell]$ cabal sdist
```

Generate and check the Pencil docs site:

```
cabal test pencil-docs
cd docs/ && python -m SimpleHTTPServer 8000
open localhost:8000
```

Note that the URLs will be broken when viewing locally. This is because GitHub
Pages deploys it to elbenshira.com/pencil, so we need links to reference /pencil.

Check that tutorials are updated.

Update the CHANGELOG.md.

Update the version number in `pencil.cabal`.

Commit the changes.

```
git commit -m "Release version 0.x.x"
```

Make sure the [Circle build is green](https://circleci.com/gh/elben/pencil).

Push to Hackage:

```
[nix-shell]$
cabal check
cabal sdist
cabal upload path/to/pencil-blah.tar.gz.

# Uploading dist/sdist/pencil-0.1.3.tar.gz...
# Package successfully uploaded as candidate. You can now preview the result at
# 'https://hackage.haskell.org/package/pencil-0.1.3/candidate'. To publish the
# candidate, use 'cabal upload --publish'.

# If the documentation build failed in the candidate page, we may need to upload
# our own docs. See https://hackage.haskell.org/upload for more info.

cabal haddock --for-hackage --hyperlink-source
cabal upload -d ./dist/pencil-x.x.x-docs.tar.gz
```

Once a candidate is published, you can test the candidate package on another project
by installing the candidate:

```
cabal install http://hackage.haskell.org/package/pencil-0.1.3/candidate/pencil-0.1.3.tar.gz

# When I tried to do this for elben.github.io, I had to run nix-shell with these packages
# included in the shell, for compiling the various Haskell dependencies through cabal.
# See: https://groups.google.com/forum/#!msg/haskell-stack/_ZBh01VP_fo/0v4SxPw7GwAJ
nix-shell -p zlib libiconv ghc
```

Finally, to publish a new version:

Tag the release:

```
git tag v0.1.0
git push --tags
```

```
[nix-shell]$
cabal sdist
cabal upload --publish pencil.tar.gz
```

## Testing against other GHC

CircleCI build also checks against other versions of GHC. To manually test it, just specify the correct .nix file:

```
nix-shell --attr env release-ghc-8.6.nix
```
