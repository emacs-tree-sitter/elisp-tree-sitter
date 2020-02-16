;;; tree-sitter.el --- Incremental parsing system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Keywords: languages tools parsers dynamic-modules tree-sitter
;; Homepage: https://github.com/ubolonton/emacs-tree-sitter
;; Version: 0.2.0
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:

;; This is an Emacs binding for tree-sitter, an incremental parsing system
;; (https://tree-sitter.github.io/tree-sitter/). It includes both the core APIs, and a minor mode
;; that provides a buffer-local up-to-date syntax tree.
;;
;; (add-to-list 'tree-sitter-major-mode-language-alist '(rust-mode . rust))
;; (add-hook 'rust-mode-hook #'tree-sitter-mode)

;;; Code:

(require 'tree-sitter-core)

(defgroup tree-sitter nil
  "Incremental parsing system."
  :group 'languages)

(defcustom tree-sitter-after-change-functions nil
  "Functions to call each time `tree-sitter-tree' is updated.
Each function will be called with a single argument: the old tree."
  :type 'hook
  :group 'tree-sitter)

(defcustom tree-sitter-major-mode-language-alist
  '((agda-mode       . agda)
    (sh-mode         . bash)
    (c-mode          . c)
    (c++-mode        . cpp)
    (css-mode        . css)
    (go-mode         . go)
    (haskell-mode    . haskell)
    (html-mode       . html)
    (java-mode       . java)
    (js-mode         . javascript)
    (js2-mode        . javascript)
    (json-mode       . json)
    (julia-mode      . julia)
    (ocaml-mode      . ocaml)
    (php-mode        . php)
    (python-mode     . python)
    (ruby-mode       . ruby)
    (rust-mode       . rust)
    (scala-mode      . scala)
    (swift-mode      . swift)
    (typescript-mode . typescript))
  "Alist that maps major modes to tree-sitter language names.
The corresponding language definitions should have been pre-installed with
tree-sitter CLI."
  :group 'tree-sitter
  :type '(alist :key-type symbol
                :value-type symbol))

(defvar-local tree-sitter-tree nil
  "Tree-sitter syntax tree.")

(defvar-local tree-sitter-parser nil
  "Tree-sitter parser.")

(defvar-local tree-sitter-language nil
  "Tree-sitter language.")

(defvar-local tree-sitter--start-byte 1)
(defvar-local tree-sitter--old-end-byte 1)
(defvar-local tree-sitter--new-end-byte 1)

(defvar-local tree-sitter--start-point [0 0])
(defvar-local tree-sitter--old-end-point [0 0])
(defvar-local tree-sitter--new-end-point [0 0])

(defun tree-sitter--before-change (beg end)
  "Update relevant editing states. Installed on `before-change-functions'.
BEG and END are the begin and end of the text to be changed."
  (setq tree-sitter--start-byte (position-bytes beg)
        tree-sitter--old-end-byte (position-bytes end))
  (ts--save-context
    ;; TODO: Keep mutating the same vectors instead of creating a new one each time.
    (setq tree-sitter--start-point (ts--point-from-position beg)
          tree-sitter--old-end-point (ts--point-from-position end))))

;;; TODO XXX: The doc says that `after-change-functions' can be called multiple times, with
;;; different regions enclosed in the region passed to `before-change-functions'. Therefore what we
;;; are doing may be a bit too naive. Several questions to investigate:
;;;
;;; 1. Are the *after* regions recorded all at once, or one after another? Are they disjointed
;;; (would imply the former)?.
;;;
;;; 2. Are the *after* regions recorded at the same time as the *before* region? If not, how can the
;;; latter possibly enclose the former, e.g. in case of inserting a bunch of text?
;;;
;;; 3. How do we batch *after* hooks to re-parse only once? Maybe using `run-with-idle-timer' with
;;; 0-second timeout?
;;;
;;; 4. What's the deal with several primitives calling `after-change-functions' *zero* or more
;;; times? Does that mean we can't rely on this hook at all?
(defun tree-sitter--after-change (_beg end _length)
  "Update relevant editing states and reparse the buffer (incrementally).
Installed on `after-change-functions'.

END is the end of the changed text."
  (setq tree-sitter--new-end-byte (position-bytes end)
        tree-sitter--new-end-point (ts-point-from-position end))
  (when tree-sitter-tree
    (ts-edit-tree tree-sitter-tree
                  tree-sitter--start-byte
                  tree-sitter--old-end-byte
                  tree-sitter--new-end-byte
                  tree-sitter--start-point
                  tree-sitter--old-end-point
                  tree-sitter--new-end-point)
    (tree-sitter--do-parse)))

(defun tree-sitter--do-parse ()
  "Parse the current buffer and update the syntax tree."
  (let ((old-tree tree-sitter-tree))
    (setq tree-sitter-tree
          ;; https://github.com/ubolonton/emacs-tree-sitter/issues/3
          (ts--without-restriction
            (ts-parse tree-sitter-parser #'ts-buffer-input tree-sitter-tree)))
    (run-hook-with-args 'tree-sitter-after-change-functions old-tree)))

(defun tree-sitter--enable ()
  "Enable `tree-sitter' in the current buffer."
  (unless tree-sitter-language
    ;; Determine the language symbol based on `major-mode' .
    (let ((lang-symbol (alist-get major-mode tree-sitter-major-mode-language-alist)))
      (unless lang-symbol
        (error "No language registered for major mode `%s'" major-mode))
      (setq tree-sitter-language (ts-require-language lang-symbol))))
  (setq tree-sitter-parser (ts-make-parser)
        tree-sitter-tree nil)
  (ts-set-language tree-sitter-parser tree-sitter-language)
  (tree-sitter--do-parse)
  (add-hook 'before-change-functions #'tree-sitter--before-change 'append 'local)
  (add-hook 'after-change-functions #'tree-sitter--after-change 'append 'local))

(defun tree-sitter--disable ()
  "Disable `tree-sitter' in the current buffer."
  (remove-hook 'before-change-functions #'tree-sitter--before-change 'local)
  (remove-hook 'after-change-functions #'tree-sitter--after-change 'local)
  (setq tree-sitter-tree nil
        tree-sitter-parser nil))

;;;###autoload
(define-minor-mode tree-sitter-mode
  "Minor mode that keeps an up-to-date syntax tree using incremental parsing."
  :init-value nil
  :lighter "tree-sitter"
  (if tree-sitter-mode
      (let ((err t))
        (unwind-protect
            (prog1 (tree-sitter--enable)
              (setq err nil))
          (when err
            (setq tree-sitter-mode nil))))
    (tree-sitter--disable)))

(provide 'tree-sitter)
;;; tree-sitter.el ends here
