# CHANGELOG

All notable changes to this project will be documented in this file.

## [Unreleased]

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
[0.1.3]: https://github.com/elben/pencil/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/elben/pencil/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/elben/pencil/compare/cb14e3610aa18dd3c71279bd56231c6bb23bae7b...v0.1.1
