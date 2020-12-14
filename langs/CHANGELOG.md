# Changelog

## Unreleased

## 0.8.0 - 2020-12-15
- Added `elm`.
- Handled underscores in language names correctly.  This enabled using `c-sharp` instead of `c_sharp`.

## 0.7.2 - 2020-10-03
- Set `tree-sitter-hl-default-patterns` to nil if there is no query file, instead of signaling an error.

## 0.7.1 - 2020-08-09
- Improved syntax highlighting for C/C++.

## 0.7.0 - 2020-07-20
- Replaced `#` with `.` in bundled highlighting queries' predicates.

## 0.6.0 - 2020-07-06
- Fixed the issue of `tree-sitter-langs` not being able to find grammars in gccemacs.
- Added `rustic-mode` to major mode mappings.
- Revamped syntax highlighting for JavaScript, TypeScript, C++.
- Improved syntax highlighting for Python, Rust.
- Improved regexp for `@constructor`.

## 0.5.0 - 2020-06-07
- Added syntax highlighting for Java.
- Improved syntax highlighting for CSS, Python.
- Removed Haskell grammar.

## 0.4.0 - 2020-05-03
- Removed grammar binaries from package file, letting them to be downloaded upon compilation instead.

## 0.2.0
- Updated grammars.
- Added highlighting queries.

## 0.1.0
Initial release
