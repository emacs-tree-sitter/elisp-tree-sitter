# ELisp Tree-sitter <img src="doc/static/img/emacs-tree-sitter-96x96.png" align="left">
[![Documentation](https://img.shields.io/badge/documentation-latest-blue)](https://emacs-tree-sitter.github.io/)
[![Azure Pipelines](https://dev.azure.com/emacs-tree-sitter/elisp-tree-sitter/_apis/build/status/ci?branchName=master&label=build&api-version=6.0-preview.1)](https://dev.azure.com/emacs-tree-sitter/elisp-tree-sitter/_build/latest?definitionId=1&branchName=master)
[![GitHub Actions](https://github.com/emacs-tree-sitter/elisp-tree-sitter/actions/workflows/main.yml/badge.svg)](https://github.com/emacs-tree-sitter/elisp-tree-sitter/actions/workflows/main.yml)

This is an Emacs Lisp binding for [tree-sitter](https://tree-sitter.github.io/tree-sitter/), an incremental parsing library. It requires Emacs 25.1 or above, built with dynamic module support.

It aims to be the foundation for a new breed of Emacs packages that understand code structurally. For example:
- Faster, fine-grained code highlighting.
- More flexible code folding.
- Structural editing (like Paredit, or even better) for non-Lisp code.
- More informative indexing for imenu.

The author of tree-sitter articulated its merits a lot better in this [Strange Loop talk](https://www.thestrangeloop.com/2018/tree-sitter---a-new-parsing-system-for-programming-tools.html).

## Installation

See the [installation section](https://emacs-tree-sitter.github.io/installation/) in the documentation.

If you want to hack on `emacs-tree-sitter` itself, see the next section instead.

## Setup for Development
**Note**: On Windows, use Powershell instead of Bash or cmd.exe.

- Clone this repo with the `--recursive` flag.
- Add 3 of its directories to `load-path`: `core/`, `lisp/` and `langs/`.
- Install [cask](https://cask.readthedocs.io).
- Run `bin/setup`.

If you want to hack on the high-level features (in Lisp) only:
- Make changes to the `.el` files.
- Add tests to `tree-sitter-tests.el` and run them with `bin/test`.

If you want to build additional (or all) grammars from source, or work on the core dynamic module, see the next 2 sections.

### Building grammars from source

**Note**: If you are only interested in building the grammar binaries, not the dynamic module, check out [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs#building-grammars-from-source).

- Install NodeJS. It is needed to generate the grammar code from the JavaScript DSL. The recommended tool to manage NodeJS is [volta](https://volta.sh/).
- Install [tree-sitter CLI tool](https://tree-sitter.github.io/tree-sitter/creating-parsers#installation): (Its binary can also be downloaded directly from [GitHub](https://github.com/tree-sitter/tree-sitter/releases).) **Note: version 0.20+ cannot be used, as they introduced [a breaking change](https://github.com/tree-sitter/tree-sitter/pull/1157) in binary storage location.**
    ```bash
    # For yarn user
    yarn global add tree-sitter-cli@0.19.3

    # For npm user
    npm install -g tree-sitter-cli@0.19.3
    ```
- Run `langs/script/compile`. For example:
    ```bash
    langs/script/compile rust
    ```

### Working on the dynamic module

- Install the [Rust toolchain](https://rustup.rs/).
- Build:
    ```bash
    bin/build
    ```
- Test:
    ```bash
    bin/test
    ```
- Continuously rebuild and test on change (requires [cargo-watch](https://github.com/passcod/cargo-watch)):
    ```bash
    bin/test watch
    ```

To test against a different version of Emacs, set the environment variable `EMACS` (e.g. `EMACS=/snap/bin/emacs make test`).

## Alternatives

- Tree-sitter in Emacs's core: https://github.com/casouri/emacs (the future)
- Binding through C instead of Rust: https://github.com/karlotness/tree-sitter.el (dormant)

## Contribution

Contributions are welcomed. Please take a look at the [issue list](https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues) for ideas, or [create a new issue](https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/new) to describe any idea you have for improvement.

For language-specific issues/features, please check out [tree-sitter-langs](https://github.com/emacs-tree-sitter/tree-sitter-langs) instead.

Show respect and empathy towards others. Both technical empathy and general empathy are highly valued.
