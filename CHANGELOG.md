# CHANGELOG

All notable changes to this project will be documented in this file.

## [Unreleased]

## [1.0.0]

### Added
- Added GHC 8.4 and 8.6 support.
- Added `(<<|)` and `coll` to add collection values into structrues.
- Added `load'` for manual file path transformation.
- Added `useFilePath`, `escapeXml`, ``rename` to manipulate loaded `Page`s.

### Changed
- Changed how structures work internally to allow collection values into structures.
- Examples now match the tutorials. This is the start of merging the tutorials into the pencil
  repo itself, instead of living somewhere else.
- Changed `load` to "magically" figure out the desired final FilePath. Use
  `load'` to manually specify the FilePath transform.
- Renamed `structure` to `struct`. It's shorter.

### Fixed
- Specify example test files in the pencil.cabal file, so that pencil tests run properly.

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

[Unreleased]: https://github.com/elben/pencil/compare/v0.1.4...HEAD
[0.1.4]: https://github.com/elben/pencil/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/elben/pencil/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/elben/pencil/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/elben/pencil/compare/cb14e3610aa18dd3c71279bd56231c6bb23bae7b...v0.1.1
