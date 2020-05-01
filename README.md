# emacs-tree-sitter
[![Build Status](https://travis-ci.org/ubolonton/emacs-tree-sitter.svg?branch=master)](https://travis-ci.org/ubolonton/emacs-tree-sitter)
[![Build Status](https://dev.azure.com/ubolonton/emacs-tree-sitter/_apis/build/status/ci?branchName=master&label=build&api-version=6.0-preview.1)](https://dev.azure.com/ubolonton/emacs-tree-sitter/_build/latest?definitionId=2&branchName=master)

This is an Emacs Lisp binding for [tree-sitter](https://tree-sitter.github.io/tree-sitter/), an incremental parsing library. It requires Emacs 25.1 or above, built with dynamic module support.

It aims to be the foundation for a new breed of Emacs packages that understand code structurally. For example:
- Faster, fine-grained code highlighting.
- More flexible code folding.
- Structural editing (like Paredit, or even better) for non-Lisp code.
- More informative indexing for imenu.

The author of tree-sitter articulated its merits a lot better in this [Strange Loop talk](https://www.thestrangeloop.com/2018/tree-sitter---a-new-parsing-system-for-programming-tools.html).

## Installation

At this stage of the project, there are few end-user-visible features, but you can already install it to play around with the APIs.

- Check that Emacs was built with module support: `(functionp 'module-load)`.
- Add the tree-sitter ELPA to `package-archives` (remember to run `package-refresh-contents` afterwards):
    ```emacs-lisp
    (add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))
    ```
- Install `tree-sitter` and `tree-sitter-langs` like other normal packages.

The package is not yet on MELPA, because it currently doesn't have a convenient way to distribute packages with pre-compiled binaries.

If you want to hack on `emacs-tree-sitter` itself, see the section [Setup for Development](#setup-for-development) instead.

## Basic Usage

- Enable the `tree-sitter` minor mode in a supported major mode (defined in `tree-sitter-major-mode-language-alist`):
    ```emacs-lisp
    (require 'tree-sitter)
    (require 'tree-sitter-langs)
    (add-hook 'rust-mode-hook #'tree-sitter-mode)
    ```
- Show the debug view of a buffer's parse tree
    ```emacs-lisp
    (require 'tree-sitter-debug)
    (tree-sitter-debug-mode)
    ```
- Get names of all functions in a Rust file:
    ```emacs-lisp
    (with-current-buffer "types.rs"
      (seq-map (lambda (capture)
                 (pcase-let ((`(_ . ,node) capture))
                   (ts-node-text node)))
               (tree-sitter-debug-query [(function_item (identifier) @name)])))
    ```
- Write a simple extension to `expand-region`:
    ```emacs-lisp
    (defun tree-sitter-mark-next-bigger-node ()
      (interactive)
      (let* ((p (point))
             (m (if mark-active (mark) p))
             (beg (min p m))
             (end (max p m))
             (root (ts-root-node tree-sitter-tree))
             (node (ts-get-named-descendant-for-position-range root beg end))
             (node-beg (ts-node-start-position node))
             (node-end (ts-node-end-position node)))
        ;; Already marking current node. Try its parent node instead.
        (when (and (= beg node-beg) (= end node-end))
          (when-let ((node (ts-get-parent node)))
            (setq node-beg (ts-node-start-position node)
                  node-end (ts-node-end-position node))))
        (set-mark node-end)
        (goto-char node-beg)))
    ```
- Parse a string:
    ```emacs-lisp
    (let ((parser (ts-make-parser)))
      (ts-set-language parser (tree-sitter-require 'rust))
      (ts-parse-string parser "fn foo() {}"))
    ```

## APIs

- The [tree-sitter doc](https://tree-sitter.github.io/tree-sitter/using-parsers) is a good read to understand its concepts, and how to use the parsers in general.
- Functions in this package are named differently, to be more Lisp-idiomatic. The overall parsing flow stays the same.
- Documentation for individual functions can be viewed with `C-h f` (`describe-function`), as usual.
- A `symbol` in the C API is actually the ID of a type, so it's called `type-id` in this package.

### Types

- `language`, `parser`, `tree`, `node`, `cursor`, `query`: corresponding tree-sitter types, embedded in `user-ptr` objects.
- `point`: a pair of `(LINE-NUMBER . BYTE-COLUMN)`.
  + `LINE-NUMBER` is the absolute line number returned by `line-number-at-pos`, counting from 1.
  + `BYTE-COLUMN` counts from 0, like `current-column`. However, unlike that function, it counts bytes, instead of displayed glyphs.
- `range`: a vector in the form of `[START-BYTEPOS END-BYTEPOS START-POINT END-POINT]`.

These types are understood only by this package. They are not recognized by `type-of`, but have corresponding type-checking predicates, which are useful for debugging: `ts-language-p`, `ts-tree-p`, `ts-node-p`...

For consistency with Emacs's conventions, this binding has some differences compared to the tree-sitter's C/Rust APIs:
- It uses 1-based byte positions, not 0-based byte offsets.
- It uses 1-based line numbers, not 0-based row coordinates.

### Functions

- Language:
    + `tree-sitter-require`: like `require`, for tree-sitter languages.
- Parser:
    + `ts-make-parser`: create a new parser.
    + `ts-set-language`: set a parser's active language.
    + `ts-parse-string`: parse a single string of source code.
    + `ts-parse-chunks`: parse source code chunks generated by an input function.
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
- Query:
    + `ts-make-query`: create a new query.
    + `ts-make-query-cursor`: create a new query cursor.
    + `ts-query-matches`, `ts-query-captures`: execute a query, returning matches/captures.

## Setup for Development

Clone this repo and add its `lisp` and `langs` directories to `load-path`.

Install [cask](https://cask.readthedocs.io) and run `cask install` to install dev dependencies.

If you want to hack on the high-level features (in Lisp) only:
- Evaluate this (once) to download the necessary binaries:
    ```emacs-lisp
    (require 'tree-sitter-langs-build)
    ;; Download pre-compiled `tree-sitter-dyn'.
    (tree-sitter-download-dyn-module)
    ;; Download pre-compiled language grammars.
    (tree-sitter-langs-install)
    ```
- Make changes to the `.el` files.
- Add tests to `tree-sitter-tests.el` and run them with `./bin/test` (`.\bin\test` on Windows).

If you want to build addtional (or all) grammars from source, or work on the core dynamic module, see the next 2 sections.

### Building grammars from source

- Install [tree-sitter CLI tool](https://tree-sitter.github.io/tree-sitter/creating-parsers#installation) (if you don't use NodeJS, you can download the binary directly from [GitHub](https://github.com/tree-sitter/tree-sitter/releases)):
    ```bash
    # For yarn user
    yarn global add tree-sitter-cli

    # For npm user
    npm install -g tree-sitter-cli
    ```
- Run:
    ```bash
    # macOS/Linux
    make ensure/rust
    ```
    ```powershell
    # Windows
    .\bin\ensure-lang rust
    ```
- You can modify`tree-sitter-langs-repos` if the language you need is not declared there.

### Working on the dynamic module

- Install the [Rust toolchain](https://rustup.rs/).
- Install `clang`, to generate the raw Rust binding for `emacs-module.h`.
- Build:
    ```bash
    # macOS/Linux
    make build
    ```
    ```powershell
    # Windows
    .\bin\build
    ```
- Test:
    ```bash
    # macOS/Linux
    make test
    ```
    ```powershell
    # Windows
    .\bin\test
    ```
- Continuously rebuild and test on change (requires [cargo-watch](https://github.com/passcod/cargo-watch)):
    ```bash
    # macOS/Linux
    make watch
    ```
    ```powershell
    # Windows
    .\bin\test watch
    ```

To test against a different version of Emacs, set the environment variable `EMACS` (e.g. `EMACS=/snap/bin/emacs make test`).

## Overall Plan

Targeting lib authors:
- Write a guide on using the tree-sitter APIs.

Targeting end users:
- Pick a language, make a "killer" minor mode that extends its major mode in multiple ways.
- Make minor modes for most common languages.
- Extract common patterns from the language minor modes into helper language-diagnostic minor modes.
- Get a language major mode to use tree-sitter for optional features.

## Alternative

Binding through C instead of Rust: https://github.com/karlotness/tree-sitter.el

## Contribution

Contributions are welcomed. Please take a look at the [issue list](https://github.com/ubolonton/emacs-tree-sitter/issues) for ideas, or [create a new issue](https://github.com/ubolonton/emacs-tree-sitter/issues/new) to describe any idea you have for improvement.

Show respect and empathy towards others. Both technical empathy and general empathy are highly valued.
