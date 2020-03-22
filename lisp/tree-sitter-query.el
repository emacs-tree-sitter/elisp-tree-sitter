;;; tree-sitter-query.el --- Tools for running queries live -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; Homepage: https://github.com/ubolonton/emacs-tree-sitter
;; Version: 0.4.0
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:

;; This file contains other debug utilities for building queries and see
;; results in a target buffer

;; Code:

(require 'scheme)
(require 'cl-lib)
(require 'tree-sitter)

(defgroup tree-sitter-query nil
  "tree-sitter playground for GNU Emacs."
  :group 'tree-sitter)

(define-derived-mode tree-sitter-query-mode prog-mode "ts-query-builder"
  "Major mode for building tree-sitter queries and testing them live"
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
  "face for match highlight"
  :group 'tree-sitter-query)

(defun tree-sitter-query--highlight-capture (capture)
  "Highlight a CAPTURE match in the current buffer."
  (pcase-let* ((`(,capture-name . ,captured-node) capture)
               (`(,node-start . ,node-end) (ts-node-position-range captured-node))
               (overlay-added (make-overlay node-start node-end)))
    ;; ensure it is deleted automatically when the overlay becomes empty
    (overlay-put overlay-added 'evaporate t)
    ;; set the match-face as the face of the overlay
    (overlay-put overlay-added 'face 'tree-sitter-query-match)
    ;; put the name of the capture in the help-echo, if any
    (unless (string= capture-name "")
      (overlay-put overlay-added 'help-echo capture-name))))

(defun tree-sitter-query--eval-query (patterns)
  "Evaluate a query PATTERNS against the target buffer."
  (with-current-buffer tree-sitter-query--target-buffer
    ;; clean the target buffer of overlays
    (remove-overlays)
    (let* ((query (ts-make-query tree-sitter-language patterns))
           (root-node (ts-root-node tree-sitter-tree))
           (captures-list (ts-query-captures query root-node)))
      (if (= (length captures-list) 0)
          (message "no matches found")
        ;; iterate all matches and highlight them with an underline
        (cl-loop
         for captures across captures-list
         do
         (cl-loop
          for capture on captures
          do (tree-sitter-query--highlight-capture capture)))))))

(defun tree-sitter-query--after-change (&rest args)
  "Run evaluation of pattern in current buffer for every change made by the user, ignoring ARGS."
  (with-current-buffer (get-buffer tree-sitter-query-builder-buffer-name)
    (let ((pattern (buffer-string)))
      (with-demoted-errors "Error: %S"
        (tree-sitter-query--eval-query pattern)))))

(defun tree-sitter-query--clean-target-buffer ()
  "Remove all overlays if the builder buffer happens to be killed."
  (with-current-buffer tree-sitter-query--target-buffer
    (remove-overlays))
  (setq tree-sitter-query--target-buffer nil))

;;;###autoload
(defun tree-sitter-query-builder ()
  "Provide means for developers to write and test tree-sitter queries.

The buffer on focus when the command is called is set as the target buffer"
  (interactive)
  (let* ((target-buffer (current-buffer))
         (builder-buffer (get-buffer-create tree-sitter-query-builder-buffer-name))
         (builder-window-is-visible (get-buffer-window builder-buffer)))
    (when (eq target-buffer builder-buffer)
      (user-error "This buffer cannot be use as target buffer"))
    (with-current-buffer target-buffer
      (unless tree-sitter-mode
        (tree-sitter-mode))
      (add-hook 'after-change-functions 'tree-sitter-query--after-change nil :local)
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
      (add-hook 'after-change-functions 'tree-sitter-query--after-change nil :local)
      (add-hook 'kill-buffer-hook 'tree-sitter-query--clean-target-buffer nil :local))
    (setf tree-sitter-query--target-buffer target-buffer)
    ;; switch focus to the query builder window
    (select-window (get-buffer-window builder-buffer))))

(provide 'tree-sitter-query)
;;; tree-sitter-query.el ends here
