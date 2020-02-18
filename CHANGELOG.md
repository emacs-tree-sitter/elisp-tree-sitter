# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]
- Changed all APIs to use Emacs's 1-based byte positions instead of 0-based byte offsets.
- Changed all APIs to use Emacs's 1-based line numbers instead of 0-based row numbering.
- Changed representation of tree-sitter point from 2-element vector to cons cell.
- Changed representation of query match/capture from 2-element vector to cons cell.

## [0.2.0] - 2020-02-02
- Upgraded `tree-sitter` to 0.6.0.
- Added `tree-sitter-cli`.
- Added `tree-sitter-langs` (utilities to download pre-compiled modules and grammars).

## [0.1.0] - 2020-01-27
Initial release

[Unreleased]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.2.0...HEAD
[0.2.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.1.0...0.2.0
