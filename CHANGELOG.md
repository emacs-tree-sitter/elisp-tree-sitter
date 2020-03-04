# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]
- Added `ts-node-position-range`.
- Upgraded `tree-sitter` to 0.6.3. This fixed `ts-type-name-for-id` and `ts-field-name-for-id` crashing on out-of-bounds IDs.
- Fixed `ts-reset-cursor` always signaling "already mutable borrowed" error.

## [0.4.0] - 2020-03-01

- Replaced functions `ts-require-language` and `ts-load-language` with `tree-sitter-require` and `tree-sitter-load`.
- Published pre-compiled `tree-sitter` through a custom ELPA.
- Published the grammar bundle `tree-sitter-langs` as a separate package.

## [0.3.0] - 2020-02-21
- Used Emacs's 1-based byte positions and line numbers instead of 0-based byte offsets and row coordinates.
- Used cons cells instead of 2-element vectors to represent tree-sitter points and query matches/captures.

## [0.2.0] - 2020-02-02
- Upgraded `tree-sitter` to 0.6.0.
- Added `tree-sitter-cli`.
- Added `tree-sitter-langs` (utilities to download pre-compiled modules and grammars).

## [0.1.0] - 2020-01-27
Initial release

[Unreleased]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.4.0...HEAD
[0.4.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.1.0...0.2.0
