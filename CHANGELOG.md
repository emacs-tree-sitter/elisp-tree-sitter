# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.8.1] - 2020-06-28
- Added customization option `tree-sitter-hl-use-font-lock-keywords`, allowing `tree-sitter-hl-mode` to work with minor modes that use `font-lock-add-keywords`.

## [0.8.0] - 2020-06-07
- Upgraded `tree-sitter` to 0.16.1. This significantly improved the expressiveness and power of tree queries.
- Made `tree-sitter-hl-mode` work without a major mode.
- Add more highlighting faces to `tree-sitter-hl`.
- Made `tree-sitter-core` automatically download `tree-sitter-dyn` binary when first compiled/loaded.
- Added [documentation](https://ubolonton.github.io/emacs-tree-sitter/).

## [0.7.0] - 2020-05-02
- Added `global-tree-sitter-mode`.
- Added library `tree-sitter-hl`, which provides query-based syntax highlighting by overriding certain parts of `font-lock`.
- Reworked query APIs for performance and clarity, most notably `ts-make-query`, `ts-query-matches`, `ts-query-captures`.
- Fixed incorrect parsing caused by misunderstandings of Emacs's clumsy change tracking machinery.

## [0.6.0] - 2020-04-11
- Renamed `ts-parse` into `ts-parse-chunks`, to avoid [conflict with `ts.el`](https://github.com/ubolonton/emacs-tree-sitter/issues/35).
- Added library `tree-sitter-query`, which enables interactively building queries.

## [0.5.0] - 2020-03-17
- Added functions `ts-node-position-range`, `ts-node-eq`.
- Added function `tree-sitter-node-at-point`.
- Added macro `tree-sitter-save-excursion`, which is useful for code formatting operations.
- Added library `tree-sitter-extras`, for extra functionalities built on top of `tree-sitter-mode`.
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
- Added library `tree-sitter-cli`.
- Added library `tree-sitter-langs` (utilities to download pre-compiled modules and grammars).

## [0.1.0] - 2020-01-27
Initial release

[Unreleased]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.8.1...HEAD
[0.8.1]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.8.0...0.8.1
[0.8.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.5.0...0.6.0
[0.5.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/ubolonton/emacs-tree-sitte/compare/0.1.0...0.2.0
