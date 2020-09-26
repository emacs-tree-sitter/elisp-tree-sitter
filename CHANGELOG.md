# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.11.0] - 2020-09-26
- Upgraded `tree-sitter` crate to [fix](https://github.com/tree-sitter/tree-sitter/pull/644) an issue where [query captures miss some nodes](https://github.com/tree-sitter/tree-sitter/issues/659). This also added a check for definitely-invalid patterns when creating a query.

## [0.10.0] - 2020-08-01
- Used keywords instead of strings for field names.
  + Replaced `ts-field-name-for-id`, `ts-field-id-for-name` with `ts-lang-field`, `ts-lang-field-id`.
  + Replaced `ts-current-field-name` with `ts-current-field`.
  + Replaced `ts-get-child-by-field-name` with `ts-get-child-by-field`.
- Used symbols for named node types.
  + Replaced `ts-type-name-for-id` with `ts-lang-node-type`.
  + Added `ts-lang-node-type-id`.
  + Changed the return type of `ts-node-type`.
- Renamed `ts-type-named-p` to `ts-lang-node-type-named-p`.
- Added optional param `NODE-TYPE` to `tree-sitter-node-at-point`.
- Upgraded `tree-sitter` crate to get support for `.not-match?` predicate.

## [0.9.2] - 2020-07-20
- Upgraded `tree-sitter` crate to add `.` as a valid start of predicates, in addition to `#`.

## [0.9.1] - 2020-07-19
- Upgraded `tree-sitter` crate to fix [incorrect capture handling](https://github.com/tree-sitter/tree-sitter/issues/685) when querying with range restriction.

## [0.9.0] - 2020-07-18
- Changed `tree-sitter-hl-add-patterns` to support language-specific patterns, in addition to buffer-local patterns.

## [0.8.3] - 2020-07-12
- Fixed incorrect highlighting when region-to-highlight's boundaries cut query patterns in halves.

## [0.8.2] - 2020-06-30
- Upgraded `tree-sitter` crate to [fix](https://github.com/tree-sitter/tree-sitter/pull/661) handling of alternations under field names.

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

[Unreleased]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.11.0...HEAD
[0.11.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.10.0...0.11.0
[0.10.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.9.2...0.10.0
[0.9.2]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.9.1...0.9.2
[0.9.1]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.9.0...0.9.1
[0.9.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.8.3...0.9.0
[0.8.3]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.8.2...0.8.3
[0.8.2]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.8.1...0.8.2
[0.8.1]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.8.0...0.8.1
[0.8.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.5.0...0.6.0
[0.5.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.4.0...0.5.0
[0.4.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/ubolonton/emacs-tree-sitter/compare/0.1.0...0.2.0
