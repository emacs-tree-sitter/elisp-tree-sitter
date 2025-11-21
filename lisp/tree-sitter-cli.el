;;; tree-sitter-cli.el --- Utilities for tree-sitter CLI -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020-2025 emacs-tree-sitter maintainers
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains functions to work with the tree-sitter CLI. It must not
;; depend (directly on indirectly) on `tsc-dyn'. It shouldn't depend on
;; `tree-sitter'.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defun tree-sitter-cli-directory ()
  "Return tree-sitter CLI's directory, including the ending separator.
This is the directory where the CLI tool keeps compiled lang definitions, among
other data."
  (file-name-as-directory
   (expand-file-name
    ;; https://github.com/tree-sitter/tree-sitter/blob/1bad6dc/cli/src/config.rs#L20
    (if-let ((dir (getenv "TREE_SITTER_DIR")))
        dir
      "~/.tree-sitter"))))

(defun tree-sitter-cli-bin-directory ()
  "Return the directory used by tree-sitter CLI to store compiled grammars."
  (file-name-as-directory
   (concat (tree-sitter-cli-directory) "bin")))

(provide 'tree-sitter-cli)
;;; tree-sitter-cli.el ends here
