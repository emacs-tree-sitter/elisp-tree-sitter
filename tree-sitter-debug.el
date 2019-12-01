;;; tree-sitter-debug.el --- Debug tools for tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains debug utilities for tree-sitter.
;;
;; (tree-sitter-debug-enable)

;;; Code:

(require 'tree-sitter)

(defvar-local tree-sitter-debug--tree-buffer nil
  "Buffer used to display the syntax tree of this buffer.")

(defun tree-sitter-debug--display-node (node depth)
  (insert (format "%s%s: \n" (make-string (* 2 depth) ?\ ) (ts-node-type node)))
  (ts-mapc-children (lambda (c)
                      (when (ts-node-named-p c)
                        (tree-sitter-debug--display-node c (1+ depth))))
                    node))

(defun tree-sitter-debug--display-tree (_old-tree)
  ;; TODO: Re-render only affected nodes.
  (when-let ((tree tree-sitter-tree))
    (with-current-buffer tree-sitter-debug--tree-buffer
      (erase-buffer)
      (tree-sitter-debug--display-node (ts-root-node tree) 0))))

(defun tree-sitter-debug-enable ()
  "Enable debugging for the current buffer.
This displays the syntax tree in another buffer, and keeps it up-to-date."
  (interactive)
  (unless tree-sitter-mode
    (error "`tree-sitter-mode' is not enabled"))
  (unless tree-sitter-debug--tree-buffer
    (setq tree-sitter-debug--tree-buffer
          (generate-new-buffer (format "*tree-sitter-tree %s*" (buffer-name)))))
  (display-buffer tree-sitter-debug--tree-buffer)
  (add-hook 'tree-sitter-after-change-functions #'tree-sitter-debug--display-tree nil 'local)
  (tree-sitter-debug--display-tree nil))

(defun tree-sitter-debug-disable ()
  "Disable debugging for the current buffer."
  (interactive)
  (when tree-sitter-debug--tree-buffer
    (kill-buffer tree-sitter-debug--tree-buffer)
    (setq tree-sitter-debug--tree-buffer nil))
  (remove-hook 'tree-sitter-after-change-functions #'tree-sitter-debug--display-tree 'local))

(defun tree-sitter-query (patterns &optional matches index-only)
  "Execute query PATTERNS against the current buffer's syntax tree and return captures.

If the optional arg MATCHES is non-nil, matches (from `ts-query-matches') are
returned instead of captures (from `ts-query-captures').

If the optional arg INDEX-ONLY is non-nil, return positions of capture patterns
within the constructed query, instead of their names.

This function is primarily useful for debugging purpose. Other packages should
build queries and cursors once, then reuse them."
  (let* ((query-str (pcase (type-of patterns)
                      ('vector (mapconcat (lambda (p) (format "%S" p)) patterns "\n"))
                      (_ (format "%S" patterns))))
         (query (ts-make-query tree-sitter-language query-str))
         (root-node (ts-root-node tree-sitter-tree)))
    (if matches
        (ts-query-matches query root-node nil index-only)
      (ts-query-captures query root-node nil index-only))))

;;; TODO: Kill tree-buffer when `tree-sitter' minor mode is turned off.

(provide 'tree-sitter-debug)
;;; tree-sitter-debug.el ends here
