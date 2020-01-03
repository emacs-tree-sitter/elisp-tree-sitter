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

(define-derived-mode tree-sitter-query-mode prog-mode "ts-query-builder"
  "Major mode for building tree-sitter queries and testing them live"
  :syntax-table scheme-mode-syntax-table
  :abbrev-table scheme-mode-abbrev-table)

(defface tree-sitter-query-match
  '((t
     (:underline
      (:color "red" :style line))))
  "face for highlighting matches")

(defvar tree-sitter-query--target-buffer nil
  "Target buffer to run the queries against")

(defun tree-sitter-query--highlight-node (node)
  "Highlight a node match in the current buffer"
  (set-text-properties (ts-node-start-position node) (ts-node-end-position node) '(face tree-sitter-query-match)))

(defun tree-sitter-query--eval-query (patterns)
  "Evaluate a query PATTERNS against the target buffer."
  (with-current-buffer tree-sitter-query--target-buffer
    (remove-text-properties (point-min) (point-max) '(face tree-sitter-query-match))
    (let* ((query (ts-make-query tree-sitter-language patterns))
           (root-node (ts-root-node tree-sitter-tree))
           (matches (ts-query-captures query root-node nil nil)))
      (if (> (length matches) 0)
          (seq-doseq (match matches)
            (tree-sitter-query--highlight-node (elt match 1)))
        (message "[ERR] no matches found or invalid query")))))

(provide 'tree-sitter-query)
;;; tree-sitter-query.el ends here
