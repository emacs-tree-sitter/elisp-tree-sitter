;;; tree-sitter-debug.el --- Debug tools for tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2025 emacs-tree-sitter maintainers
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Maintainer: Jen-Chieh Shen <jcs090218@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains debug utilities for tree-sitter.
;;
;; (tree-sitter-debug-mode)

;;; Code:

(require 'tree-sitter)

(require 'generator)

(defvar-local tree-sitter-debug--tree-buffer nil
  "Buffer used to display the syntax tree of this buffer.")

(defvar-local tree-sitter-debug--source-code-buffer nil
  "Source buffer of the syntax tree displayed in this `tree-sitter-debug' buffer.")

(defgroup tree-sitter-debug nil
  "Tree sitter debug and display features."
  :group 'tree-sitter)

(defcustom tree-sitter-debug-jump-buttons nil
  "Whether to enable jump-to-node buttons in `tree-sitter-debug' views.
This can have a performance penalty in large buffers."
  :type 'boolean
  :group 'tree-sitter-debug)

(defcustom tree-sitter-debug-highlight-jump-region nil
  "Whether to highlight the node jumped to.
This only takes effect if `tree-sitter-debug-jump-buttons' is non-nil."
  :type 'boolean
  :group 'tree-sitter-debug)

(defun tree-sitter-debug--button-node-lookup (button)
  "The function to call when a `tree-sitter-debug' BUTTON is clicked."
  (unless tree-sitter-debug--source-code-buffer
    (error "No source code buffer set"))
  (unless (buffer-live-p tree-sitter-debug--source-code-buffer)
    (user-error "Source code buffer has been killed"))
  (unless button
    (user-error "This function must be called on a button"))
  (tree-sitter-debug--goto-node tree-sitter-debug--source-code-buffer
                                (button-get button 'points-to)))

(defun tree-sitter-debug--goto-node (buffer byte-range)
  "Switch to BUFFER, centering on the region defined by NODE."
  (switch-to-buffer-other-window buffer)
  (goto-char (byte-to-position (car byte-range)))
  (push-mark (byte-to-position (cdr byte-range))
             tree-sitter-debug-highlight-jump-region))

(defun tree-sitter-debug--display-node (named-p type start-byte end-byte depth field)
  "Display NODE that appears at the given DEPTH in the syntax tree."
  (when named-p
    (insert (make-string (* 2 depth) ?\ ))
    (let* ((field-text (if field
                           (format " (%s)" field)
                         ""))
           (node-text (format "%s%s:" type field-text)))
      (if tree-sitter-debug-jump-buttons
          (insert-button node-text
                         'action 'tree-sitter-debug--button-node-lookup
                         'follow-link t
                         'points-to `(,start-byte . ,end-byte))
        (insert node-text))
      (insert "\n"))))

(defvar tree-sitter-debug-traversal-method :mapc)

(defun tree-sitter-debug--display-tree (_old-tree)
  "Display the current `tree-sitter-tree'."
  ;; TODO: Re-render only affected nodes.
  (when-let ((tree tree-sitter-tree))
    (with-current-buffer tree-sitter-debug--tree-buffer
      (let (buffer-read-only)
        (erase-buffer)
        (pcase tree-sitter-debug-traversal-method
          (:mapc (tsc-traverse-mapc
                  (lambda (props)
                    (pcase-let ((`[,named-p ,type ,start-byte ,end-byte ,depth ,field] props))
                      (tree-sitter-debug--display-node
                       named-p type start-byte end-byte depth field)))
                  tree
                  [:named-p :type :start-byte :end-byte :depth :field]))
          (:iter (iter-do (props (tsc-traverse-iter
                                  tree [:named-p :type :start-byte :end-byte :depth :field]))
                   (pcase-let ((`[,named-p ,type ,start-byte ,end-byte ,depth ,field] props))
                     (tree-sitter-debug--display-node
                      named-p type start-byte end-byte depth field))))
          (:do (tsc-traverse-do ([named-p type start-byte end-byte depth field] tree)
                 (tree-sitter-debug--display-node
                  named-p type start-byte end-byte depth field))))))))

(defun tree-sitter-debug--setup ()
  "Set up syntax tree debugging in the current buffer."
  (unless (buffer-live-p tree-sitter-debug--tree-buffer)
    (setq tree-sitter-debug--tree-buffer
          (get-buffer-create (format "*tree-sitter-tree: %s*" (buffer-name)))))
  (let ((source-buffer (current-buffer)))
    (with-current-buffer tree-sitter-debug--tree-buffer
      (buffer-disable-undo)
      (setq tree-sitter-debug--source-code-buffer source-buffer
            buffer-read-only t)))
  (add-hook 'tree-sitter-after-change-functions #'tree-sitter-debug--display-tree nil :local)
  (add-hook 'kill-buffer-hook #'tree-sitter-debug--teardown nil :local)
  (display-buffer tree-sitter-debug--tree-buffer)
  (tree-sitter-debug--display-tree nil))

(defun tree-sitter-debug--teardown ()
  "Tear down syntax tree debugging in the current buffer."
  (remove-hook 'tree-sitter-after-change-functions #'tree-sitter-debug--display-tree :local)
  (when (buffer-live-p tree-sitter-debug--tree-buffer)
    (kill-buffer tree-sitter-debug--tree-buffer)
    (setq tree-sitter-debug--tree-buffer nil)))

;;;###autoload
(define-minor-mode tree-sitter-debug-mode
  "Toggle syntax tree debugging for the current buffer.
This mode displays the syntax tree in another buffer, and keeps it up-to-date."
  :init-value nil
  :group 'tree-sitter
  (tree-sitter--handle-dependent tree-sitter-debug-mode
    #'tree-sitter-debug--setup
    #'tree-sitter-debug--teardown))

;;;###autoload
(defun tree-sitter-debug-query (patterns &optional matches tag-assigner)
  "Execute query PATTERNS against the current syntax tree and return captures.

If the optional arg MATCHES is non-nil, matches (from `tsc-query-matches') are
returned instead of captures (from `tsc-query-captures').

If the optional arg TAG-ASSIGNER is non-nil, it is passed to `tsc-make-query' to
assign custom tags to capture names.

This function is primarily useful for debugging purpose. Other packages should
build queries and cursors once, then reuse them."
  (let* ((query (tsc-make-query tree-sitter-language patterns tag-assigner))
         (root-node (tsc-root-node tree-sitter-tree)))
    (tsc--without-restriction
      (if matches
          (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties)
        (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))))

;;; TODO: Kill tree-buffer when `tree-sitter' minor mode is turned off.

(provide 'tree-sitter-debug)
;;; tree-sitter-debug.el ends here
