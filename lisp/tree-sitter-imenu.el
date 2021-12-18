;;; tree-sitter-imenu.el --- Jump to buffer points of note, in the current buffer, based on tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;;         Timo von Hartz <c0untlizzi@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file adds an `imenu' backend using `tree-sitter'.
;;
;; This lets users call `imenu' to interactively select relevent
;; points of interest in the current buffer such as class declarations
;; or variable assignments.

;;; Code:

(require 'tree-sitter)

(eval-when-compile
  (require 'cl-lib))

(defgroup tree-sitter-imenu nil
  "`imenu' using tree-sitter."
  :group 'tree-sitter)

;;; ----------------------------------------------------------------------------
;;; Interfaces for modes and end users.

(defvar-local tree-sitter-imenu--query nil
  "Tree query used for `imenu', compiled from patterns.")

(defvar-local tree-sitter-imenu-default-patterns nil
  "Default imenu patterns.
This should be set by major modes that want to integrate with `tree-sitter-imenu'.
It plays a similar role to `imenu-generic-expression'.

It is either a string, or a vector of S-expressions. For more details on the
syntax, see https://emacs-tree-sitter.github.io/syntax-highlighting/queries/.")

(defvar tree-sitter-imenu--patterns-alist nil
  "Additional language-specific `imenu' patterns.
It plays a similar role to `font-lock-keywords-alist', except that its keys are
language symbols, not major mode symbols.")
(put 'tree-sitter-imenu--patterns-alist 'risky-local-variable t)

(defvar-local tree-sitter-imenu--extra-patterns-list nil
  "Additional buffer-local syntax highlighting patterns.")

(defcustom tree-sitter-imenu-type-matching-function
  #'identity
  "Function used to map capture names in query patterns to `imenu' types.
This can also be used to selectively disable certain capture names."
  :group 'tree-sitter-imenu
  :type 'function)

(defun tree-sitter-imenu--query ()
  (or tree-sitter-imenu--query
      (setq tree-sitter-imenu--query
            (when tree-sitter-imenu-default-patterns
              (tsc-make-query
               tree-sitter-language
               (mapconcat #'tsc--stringify-patterns
                          (append tree-sitter-imenu--extra-patterns-list
                                  (alist-get (tsc--lang-symbol tree-sitter-language)
                                             tree-sitter-imenu--patterns-alist)
                                  (list tree-sitter-imenu-default-patterns))
                          "\n")
               tree-sitter-imenu-type-matching-function)))))

;; TODO: tree-sitter-imenu-add-patterns

;;; ----------------------------------------------------------------------------
;;; Index function

(defun tree-sitter-imenu-index-function ()
  (thread-last (tsc--query-cursor-captures
                nil
                (tree-sitter-imenu--query)
                (tsc-root-node tree-sitter-tree)
                #'tsc--buffer-substring-no-properties)
               ;; Group each match by its capture within imenu.
               (seq-group-by
                (lambda (capture) (car capture)))
               ;; Convert each capture response into an imenu node.
               (seq-map (lambda (captures)
                          (cons (car captures)
                                (cl-loop for (type . node) in (cdr captures)
                                         collect
                                         (cons (ts-node-text node)
                                               (ts-node-start-position node))))))))

;;; ----------------------------------------------------------------------------
;;; Setup and teardown.

(defun tree-sitter-imenu--setup ()
  (when (tree-sitter-imenu--query)
    (setq imenu-create-index-function
          #'tree-sitter-imenu-index-function))
  )

(defun tree-sitter-imenu--teardown ()
  ;; TODO: Maybe restore previous index function?
  (when (eq imenu-create-index-function
            #'tree-sitter-imenu-index-function)
    (kill-local-variable 'imenu-create-index-function)))

(define-minor-mode tree-sitter-imenu-mode
  "Toggle `imenu' support based on Tree-sitter's syntax tree.
If `tree-sitter-imenu-default-patterns' is nil, turning on this mode does nothing,
and does not interfere with `font-lock-mode'.

Enabling this automatically enables `tree-sitter-mode' in the buffer.

To enable this automatically whenever `tree-sitter-mode' is enabled:

 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-imenu-mode)"
  :init-value nil
  :group 'tree-sitter
  (tree-sitter--handle-dependent tree-sitter-imenu-mode
    #'tree-sitter-imenu--setup
    #'tree-sitter-imenu--teardown))

(provide 'tree-sitter-imenu)
;;; tree-sitter-imenu.el ends here
