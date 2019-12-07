;; tree-sitter-builder.el --- tools for running queries live -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>

;; This file contains other debug utilities for building queries and see
;; results in a target buffer

;; Code:

(require 'scheme)
(require 'tree-sitter)
(require 'seq)

(define-derived-mode tree-sitter-builder-mode prog-mode "ts-query-builder"
  "Major mode for building tree-sitter queries and testing them live"
  :syntax-table scheme-mode-syntax-table
  :abbrev-table scheme-mode-abbrev-table)

(defface tree-sitter-builder-match
  '((t
     (:underline
      (:color "red" :style line))))
  "face for highlighting matches")

(defvar tree-sitter-builder--target-buffer nil
  "Target buffer to run the queries against")

(defun tree-sitter-builder--highlight-node (node)
  "Highlight a NODE match in the current buffer."
  (let ((_node (elt node 1)))
    (add-text-properties (ts-node-start-position _node) (ts-node-end-position _node) '(font-lock-face tree-sitter-builder-match))))

(defun tree-sitter-builder--eval-query (patterns)
  "Evaluate a query PATTERNS against the target buffer."
  (with-current-buffer tree-sitter-builder--target-buffer
    (remove-text-properties (point-min) (point-max) '(font-lock-face tree-sitter-builder-match))
    (let* ((query (ts-make-query tree-sitter-language patterns))
           (root-node (ts-root-node tree-sitter-tree))
           (matches (ts-query-captures query root-node nil nil)))
      (if (> (length matches) 0)
          (seq-doseq (match matches)
            (tree-sitter-builder--highlight-node match))
        (message "[ERR] no matches found or invalid query")))))

(provide 'tree-sitter-builder)
;;; tree-sitter-builder.el ends here
