;; tree-sitter-query.el --- tools for running queries live -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>

;; This file contains other debug utilities for building queries and see
;; results in a target buffer

;; Code:

(require 'scheme)
(require 'cl-lib)
(require 'tree-sitter)
(require 'tree-sitter-query-faces)

(define-derived-mode tree-sitter-query-mode prog-mode "ts-query-builder"
  "Major mode for building tree-sitter queries and testing them live"
  :syntax-table scheme-mode-syntax-table
  :abbrev-table scheme-mode-abbrev-table)

(defvar tree-sitter-query--target-buffer nil
  "Target buffer to run the queries against.")

(defvar tree-sitter-query--match-highlight-number 0
  "Counter that keeps track of the number of the match color face.")

(defvar tree-sitter-query--match-highlight-text-property-used '()
  "List which text properties where used.")

(defun tree-sitter-query--get-next-match-highlight-color ()
  "Return the symbol for the next highlight match face in number order."
  (when (>= tree-sitter-query--match-highlight-number 114)
    (setq tree-sitter-query--match-highlight-number 0))
  ;; add 1 to the variable
  (setq tree-sitter-query--match-highlight-number (+ tree-sitter-query--match-highlight-number 1))
  (elt tree-sitter-query-faces-match-list tree-sitter-query--match-highlight-number))

(defun tree-sitter-query--highlight-node (node match-face)
  "Highlight a NODE match in the current buffer with face MATCH-FACE."
  (let* ((capture-name (car node))
         (captured-node (cdr node))
         (node-start (ts-node-start-position captured-node))
         (node-end (ts-node-end-position captured-node))
         (font-lock-face-property `(font-lock-face ,match-face))
         (help-echo-property `(help-echo ,capture-name)))
    ;; register which properties were added to the region of text to aid on the
    ;; removal on the next pattern evaluation
    (add-to-list 'tree-sitter-query--match-highlight-text-property-used font-lock-face-property)
    ;; add the font-lock face
    (add-text-properties node-start node-end font-lock-face-property)
    ;; and add the name of the capture node, if any.
    (unless (string= capture-name "")
      (add-to-list 'tree-sitter-query--match-highlight-text-property-used help-echo-property)
      (add-text-properties node-start node-end help-echo-property))))

(defun tree-sitter-query--eval-query (patterns)
  "Evaluate a query PATTERNS against the target buffer."
  (with-current-buffer tree-sitter-query--target-buffer
    ;; clean the target buffer of properties previously added
    (dolist (property tree-sitter-query--match-highlight-text-property-used)
      (remove-text-properties (point-min) (point-max) property))
    ;; clear the list of used properties
    (setq tree-sitter-query--match-highlight-text-property-used '())
    (let* ((query (ts-make-query tree-sitter-language patterns))
           (root-node (ts-root-node tree-sitter-tree))
           (matches (ts-query-captures query root-node nil nil))
           (nextface (tree-sitter-query--get-next-match-highlight-color))
           (capture-name ""))
      (if (> (length matches) 0)
          (progn
            ;; reset counter
            (setq tree-sitter-query--match-highlight-number 0)
            ;; iterate all matches and highlight them with an underline
            (cl-loop
             for submatches across matches
             do (cl-loop
                 for match on submatches
                 do (unless (string= capture-name (car match))
                      (setq capture-name (car match))
                      (setq nextface (tree-sitter-query--get-next-match-highlight-color)))
                 (tree-sitter-query--highlight-node match nextface))))
        (message "[ERR] no matches found or invalid query")))))

(defun tree-sitter-query--after-change (&rest args)
  "Run evaluation of pattern in current buffer for every change made by the user, ignoring ARGS."
  (let ((pattern (buffer-substring-no-properties (point-min) (point-max))))
    (with-demoted-errors
        (tree-sitter-query--eval-query pattern))))

(defun tree-sitter-query-builder ()
  "Provide means for developers to write and test tree-sitter queries.

The buffer on focus when the command is called is set as the target buffer"
  (interactive)
  (let* ((target-buffer (current-buffer))
         (builder-buffer (get-buffer-create "*tree-sitter-query-builder*"))
         (builder-window-is-visible (get-buffer-window builder-buffer))
         (builder-window))
    (when (eq target-buffer builder-buffer)
      (error "This buffer cannot be use as target buffer"))
    (unless builder-window-is-visible
      (setf builder-window (split-window-vertically -7))
      (with-selected-window builder-window
        (switch-to-buffer builder-buffer)))
    (with-current-buffer target-buffer
      (unless tree-sitter-mode
        (tree-sitter-mode)))
    (with-current-buffer builder-buffer
      (erase-buffer)
      (tree-sitter-query-mode)
      (add-hook 'after-change-functions 'tree-sitter-query--after-change nil t))
    (setf tree-sitter-query--target-buffer target-buffer)
    (select-window builder-window)))

(provide 'tree-sitter-query)
;;; tree-sitter-query.el ends here
