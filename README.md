# emacs-tree-sitter [![Build Status](https://travis-ci.org/ubolonton/emacs-tree-sitter.svg?branch=master)](https://travis-ci.org/ubolonton/emacs-tree-sitter) [![Build Status](https://dev.azure.com/ubolonton/emacs-tree-sitter/_apis/build/status/ubolonton.emacs-tree-sitter?branchName=master)](https://dev.azure.com/ubolonton/emacs-tree-sitter/_build/latest?definitionId=2&branchName=master)

This is an Emacs Lisp binding for [tree-sitter](https://tree-sitter.github.io/tree-sitter/), an incremental parsing library.

It aims to be the foundation for a new breed of Emacs packages that understand code structurally. For example:
- Faster, fine-grained code highlighting.
- More flexible code folding.
- Structural editing (like Paredit, or even better) for non-Lisp code.
- More informative indexing for imenu.

The author of tree-sitter articulated its merits a lot better in this [Strange Loop talk](https://www.thestrangeloop.com/2018/tree-sitter---a-new-parsing-system-for-programming-tools.html).

## Pre-requisites

- Emacs 25.1 or above, built with module support. This can be checked with `(functionp 'module-load)`.
- [Rust toolchain](https://rustup.rs/), to build the dynamic module.
- [tree-sitter CLI tool](https://tree-sitter.github.io/tree-sitter/creating-parsers#installation), to build loadable language files from grammar repos.
- `clang`, to generate the raw Rust binding for `emacs-module.h`.

## Building and Installation

- Clone this repo.
- Build:
    ```bash
    make build
    ```
- Add this repo's directory to `load-path`.

## Getting Language Files
This package is currently not bundled with any language. Parsers for individual languages are to be built and loaded from shared dynamic libraries, using `tree-sitter` CLI tool.

- Install `tree-sitter` CLI tool (if you don't use NodeJS, you can download the binary directly from [GitHub](https://github.com/tree-sitter/tree-sitter/releases)):
    ```bash
    # For yarn user
    yarn global add tree-sitter-cli

    # For npm user
    npm install -g tree-sitter-cli
    ```
- Use the wrapper script [`bin/ensure-lang`](bin/ensure-lang) to download the grammar from [tree-sitter's GitHub org](https://github.com/tree-sitter) and build the parser:
    ```bash
    # The shared lib will be at ~/.tree-sitter/bin/rust.so (.dll on Windows)
    make ensure/rust
    ```
- Load it in Emacs:
    ```emacs-lisp
    (require 'tree-sitter)
    (ts-require-language 'rust)
    ```

## Basic Usage

- Enable `tree-sitter` in a major mode:
    ```emacs-lisp
    (require 'tree-sitter)
    ;;; Assuming ~/.tree-sitter/bin/rust.so was already generated.
    (add-hook 'rust-mode-hook #'tree-sitter-mode)
    ```
- Show the debug view of a buffer's parse tree:
    ```emacs-lisp
    (require 'tree-sitter-debug)
    (tree-sitter-debug-enable)
    ```
- Customize the language to use for a major mode:
    ```emacs-lisp
    (add-to-list 'tree-sitter-major-mode-language-alist '(scala-mode . scala))
    ```
- Use the lower-level APIs directly:
    ```emacs-lisp
    (setq rust (ts-require-language 'rust))
    (setq parser (ts-make-parser))
    (ts-set-language parser rust)

    ;;; Parse a simple string.
    (ts-parse-string parser "fn foo() {}")

    ;;; Incremental parsing.
    (with-temp-buffer
      (insert-file-contents "src/types.rs")
      (let* ((tree)
             (initial (benchmark-run (setq tree (ts-parse parser #'ts-buffer-input nil))))
             (reparse (benchmark-run (ts-parse parser #'ts-buffer-input tree))))
        ;; Second parse should be much faster than the initial parse, especially as code size grows.
        (message "initial %s" initial)
        (message "reparse %s" reparse)))
    ```

## APIs

- The [tree-sitter doc](https://tree-sitter.github.io/tree-sitter/using-parsers) is a good read to understand its concepts, and how to use the parsers in general.
- Functions in this package are named differently, to be more Lisp-idiomatic. The overall parsing flow stays the same.
- Documentation for individual functions can be viewed with `C-h f` (`describe-function`), as usual.
- A `symbol` in the C API is actually the ID of a type, so it's called `type-id` in this package.

### Types

- `language`, `parser`, `tree`, `node`, `cursor`: corresponding tree-sitter types, embedded in `user-ptr` objects.
- `point`: a vector in the form of `[row column]`, where `row` and `column` are zero-based. This is different from Emacs's concept of "point". Also note that `column` counts bytes, unlike the current built-in function `current-column`.
- `range`: a vector in the form of `[start-point end-point]`.

These types are understood only by this package. They are not recognized by `type-of`, but have corresponding type-checking predicates, which are useful for debugging: `ts-language-p`, `ts-tree-p`, `ts-node-p`...

### Functions

- Language:
    + `ts-require-language`: like `require`, for tree-sitter languages.
- Parser:
    + `ts-make-parser`: create a new parser.
    + `ts-set-language`: set a parser's active language.
    + `ts-parse-string`: parse a string.
    + `ts-parse`: parse with a text-generating callback.
    + `ts-set-included-ranges`: set sub-ranges when parsing multi-language text.
- Tree:
    + `ts-root-node`: get the tree's root node.
    + `ts-edit-tree`: prepare a tree for incremental parsing.
    + `ts-changed-ranges`: compare 2 trees for changes.
    + `ts-tree-to-sexp`: debug utility.
- Cursor:
    + `ts-make-cursor`: obtain a new cursor from either a tree or a node.
    + `ts-goto-` functions: move to a different node.
    + `ts-current-` functions: get the current field/node.
- Node:
    + `ts-node-` functions: node's properties and predicates.
    + `ts-get-` functions: get related nodes (parent, siblings, children, descendants).
    + `ts-count-` functions: count child nodes.
    + `ts-mapc-children`: loops through child nodes.
    + `ts-node-to-sexp`: debug utility.

## Development
- Testing:
    ```bash
    make test
    ```
- Continuous testing (requires [cargo-watch](https://github.com/passcod/cargo-watch)):
    ```shell
    make watch
    ```

On Windows, use PowerShell to run the corresponding `.ps1` scripts.

## Alternative
Binding through C instead of Rust: https://github.com/karlotness/tree-sitter.el
