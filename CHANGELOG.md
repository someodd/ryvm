# Changelog

## [Unreleased]

## [0.2.0.0] - 2025-08-30

### Added
- Command-line arguments for `LOCATION` and `QUERY` are now the default way to use `ryvm`.
- `--stdin` flag to read `LOCATION` and `QUERY` from standard input, preserving the old behavior.
- `--make-relative` flag to make result file paths relative to the search location.
- `--ext-whitelist` flag to specify a comma-separated list of file extensions to search.

### Changed
- **BREAKING**: The primary command-line interface has been changed from reading from stdin to taking `LOCATION` and `QUERY` as arguments.

## [0.1.0.0] - 2025-08-27

### Added
- Initial release

[unreleased]: https://github.com/someodd/ryvm/compare/v0.2.0.0...HEAD
[0.2.0.0]: https://github.com/someodd/ryvm/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/someodd/ryvm/releases/tag/v0.1.0.0
