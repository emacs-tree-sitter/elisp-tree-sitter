;;; tree-sitter-query.el --- Tools for running queries live -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020-2026 emacs-tree-sitter maintainers
;;
;; Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains other debug utilities for building queries and see results
;; in a target buffer.

;;; Code:

(require 'scheme)
(require 'tree-sitter)

(defgroup tree-sitter-query nil
  "Tree-sitter playground."
  :group 'tree-sitter)

(define-derived-mode tree-sitter-query-mode prog-mode "tsc-query-builder"
  "Major mode for building tree-sitter queries and testing them live."
  :syntax-table scheme-mode-syntax-table
  :abbrev-table scheme-mode-abbrev-table)

(defconst tree-sitter-query-builder-buffer-name "*tree-sitter-query-builder*"
  "Name of the builder buffer.")

(defvar tree-sitter-query--target-buffer nil
  "Target buffer to run the queries against.")

(defface tree-sitter-query-match
  '((t :foreground "#000"
       :background "#00bfff"
       :weight bold))
  "Face for highlight captures in matches."
  :group 'tree-sitter-query)

(defun tree-sitter--echo (&rest args)
  "Display a transient message, without logging it in the `*Messages*' buffer."
  (let (message-log-max)
    (apply #'message args)))

(defun tree-sitter-query--highlight-capture (capture)
  "Highlight CAPTURE in the current buffer."
  (pcase-let* ((`(,capture-symbol . ,captured-node) capture)
               (`(,node-start . ,node-end) (tsc-node-position-range captured-node))
               (overlay (make-overlay node-start node-end))
               (capture-name (symbol-name capture-symbol)))
    ;; Ensure the overlay is deleted when it becomes empty.
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'face 'tree-sitter-query-match)
    ;; Use the capture's name as the mouseover tooltip.
    (unless (string= capture-name "")
      (overlay-put overlay 'help-echo capture-name))))

(defun tree-sitter-query--eval-query (patterns)
  "Evaluate query PATTERNS against the target buffer."
  (with-current-buffer tree-sitter-query--target-buffer
    (tsc--without-restriction
      (remove-overlays)
      (when-let*
          ((query
            (condition-case err
                (tsc-make-query tree-sitter-language patterns)
              ((tsc-query-invalid-node-type
                tsc-query-invalid-field
                tsc-query-invalid-capture)
               (tree-sitter--echo "%s: %s" (get (car err) 'error-message) (cadr err))
               nil)
              (tsc-query-invalid
               (tree-sitter--echo "%s" (get (car err) 'error-message))
               nil)))
           (root-node (tsc-root-node tree-sitter-tree))
           (captures (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))
        (if (= (length captures) 0)
            (tree-sitter--echo "No captures found")
          (mapc #'tree-sitter-query--highlight-capture captures))))))

(defun tree-sitter-query--after-change (&rest _args)
  "Run query patterns against the target buffer and update highlighted texts."
  (with-current-buffer (get-buffer tree-sitter-query-builder-buffer-name)
    (let ((patterns (buffer-string)))
      (with-demoted-errors "Error: %S"
        (tree-sitter-query--eval-query patterns)))))

(defun tree-sitter-query--clean-target-buffer ()
  "Remove all overlays from the target buffer."
  (with-current-buffer tree-sitter-query--target-buffer
    (remove-overlays))
  (setq tree-sitter-query--target-buffer nil))

;;;###autoload
(defun tree-sitter-query-builder ()
  "Provide means for developers to write and test tree-sitter queries.

The buffer on focus when the command is called is set as the target buffer."
  (interactive)
  (let* ((target-buffer (current-buffer))
         (builder-buffer (get-buffer-create tree-sitter-query-builder-buffer-name))
         (builder-window-is-visible (get-buffer-window builder-buffer)))
    (when (eq target-buffer builder-buffer)
      (user-error "This buffer cannot be use as target buffer"))
    (with-current-buffer target-buffer
      (unless tree-sitter-mode
        (tree-sitter-mode))
      ;; TODO: The query should be run against the changed range only.
      (add-hook 'tree-sitter-after-change-functions #'tree-sitter-query--after-change nil :local)
      (setq tree-sitter-query--target-buffer target-buffer))
    (unless builder-window-is-visible
      (unless (display-buffer-in-side-window
               builder-buffer
               '((side . bottom)
                 (window-height . 10)))
        (user-error "Not enough space available for query builder window")))
    (with-current-buffer builder-buffer
      (erase-buffer)
      (tree-sitter-query-mode)
      (add-hook 'after-change-functions #'tree-sitter-query--after-change nil :local)
      (add-hook 'kill-buffer-hook #'tree-sitter-query--clean-target-buffer nil :local))
    (setf tree-sitter-query--target-buffer target-buffer)
    ;; Switch focus to the query builder window.
    (select-window (get-buffer-window builder-buffer))))

(provide 'tree-sitter-query)
;;; tree-sitter-query.el ends here
