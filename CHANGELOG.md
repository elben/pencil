# CHANGELOG

All notable changes to this project will be documented in this file.

## [Unreleased]

## [1.0.0]

This is a milestone release! Version 1.0.0. Several breaking changes, but if you
read through the changes below, you should find updating your code to be pretty
easy. Please email me if you are having problems!

### Added
- Added GHC 8.4 and 8.6 support.
- Added `(<<|)` and `coll` to add collection values into structrues.
- Added `useFilePath`, `escapeXml`, `rename`, `to`, `move` to manipulate loaded `Page`s.
- Added `loadAndRender` convenience method. Supports individual files and
  directories. You'll want to use this one to move over static assets quickly
  and easily: `loadAndRender "images/"`.
- Added `toTextRss` and `rfc822DateFormat` to render content ready for an RSS
  template. See the Blog example for details.

### Changed
- All of the `Pencil.Blog` functions are now re-exported in `Pencil`. So you
  only need to `import Pencil` now.
- `load` now automatically figures out the desired final FilePath, so it
  doesn't take a `(FilePath -> FilePath)` as the first argument anymore. You
  can change your code from `load toHtml "foo.markdown"` to `load
  "foo.markdown"`, for the most part. Use `load'` to manually specify the
  FilePath transform. See examples in the Hackage docs for `load'`, `to`,
  `move` and `rename`.
- `loadResources`, like `load`, no longers takes a file path transformer. Use
  `to`, `move` or `rename` to change the file path. But really, you probably can
  use `loadDir` or `loadDir'` or `loadAndRender` instead of `loadResources.`
- Renamed `structure` to `struct`. It's shorter.
- `passthrough` now works with directories too.
- `insertPages` return type changed from `Env` to `PencilApp Env`. We now
  evaluate the given pages (e.g. replace variables) before inserting into
  env. So you'll need to change from `let env' = insertPages "posts" posts env`
  to `env <- insertPages "posts" posts env`.
- Renamed `updateEnvVal` to `adjust`.
- Renamed `insertEnv` to `insert`.
- Renamed `injectTagsEnv` to `injectTags`.
- Renamed `arrayContainsString` to `arrayContainsText`.
- Changed how structures work internally to allow collection values into structures.
- Examples now match the tutorials. This is the start of merging the tutorials into the pencil
  repo itself, instead of living somewhere else.
- Two new errors: `CollectionNotLastInStructure` and
  `CollectionFirstInStructure` to handle collection positions in structures.

### Fixed
- Specify example test files in the pencil.cabal file, so that pencil tests run properly.

### Removed
- `renderCss`. Use `loadAndRender` instead. As in: `loadAndRender "style.scss"`
- Removed `VarNotInEnv` error type, since Pencil no longer throws that.

## [0.1.3]

### Changed
- Updated dependencies. Should be able to use with recent versions of Stack LTS releases and Nix channels.
- Pandoc updated to 2.5 from 1.x. Source code renders using `<a>` tags now, so you may have to change your CSS. If you want
  CSS to target only `<a href>` tags, use `a[href] { ... }`.

## [0.1.2]

### Added
- Escape template directives using `$${example}`. This will be rendered
  literally as `${example}`.

## [0.1.1]

### Added
- Blog example.
- Minor method changes.

### Changed
- Bounds changed for ghc 8.0.2 and 8.2.2 support.
- Improved documentation.

## [0.1.0](https://github.com/elben/pencil/)

### Added
- First release.

## [0.0.0](https://github.com/elben/pencil/)

### Added
### Changed
### Fixed
### Removed
### Deprecated
### Security

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/).

[Unreleased]: https://github.com/elben/pencil/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/elben/pencil/compare/v0.1.3...v1.0.0
[0.1.3]: https://github.com/elben/pencil/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/elben/pencil/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/elben/pencil/compare/cb14e3610aa18dd3c71279bd56231c6bb23bae7b...v0.1.1
