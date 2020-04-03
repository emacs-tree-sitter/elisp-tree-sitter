;;; tree-sitter.el --- Incremental parsing system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Keywords: languages tools parsers dynamic-modules tree-sitter
;; Homepage: https://github.com/ubolonton/emacs-tree-sitter
;; Version: 0.5.0
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:

;; This is an Emacs binding for tree-sitter, an incremental parsing system
;; (https://tree-sitter.github.io/tree-sitter/). It includes both the core APIs,
;; and a minor mode that provides a buffer-local up-to-date syntax tree.

;;; Code:

(require 'tree-sitter-core)
(require 'tree-sitter-load)

(defgroup tree-sitter nil
  "Incremental parsing system."
  :group 'languages)

(defcustom tree-sitter-after-change-functions nil
  "Functions to call each time `tree-sitter-tree' is updated.
Each function will be called with a single argument: the old tree."
  :type 'hook
  :group 'tree-sitter)

(defcustom tree-sitter-major-mode-language-alist nil
  "Alist that maps major modes to tree-sitter language names."
  :group 'tree-sitter
  :type '(alist :key-type symbol
                :value-type symbol))

(defvar-local tree-sitter-tree nil
  "Tree-sitter syntax tree.")

(defvar-local tree-sitter-parser nil
  "Tree-sitter parser.")

(defvar-local tree-sitter-language nil
  "Tree-sitter language.")

(defvar-local tree-sitter--start-byte nil)
(defvar-local tree-sitter--old-end-byte nil)
(defvar-local tree-sitter--new-end-byte nil)

(defvar-local tree-sitter--start-point nil)
(defvar-local tree-sitter--old-end-point nil)
(defvar-local tree-sitter--new-end-point nil)

(defun tree-sitter--before-change (beg end)
  "Update relevant editing states. Installed on `before-change-functions'.
BEG and END are the begin and end of the text to be changed."
  (setq tree-sitter--start-byte (position-bytes beg)
        tree-sitter--old-end-byte (position-bytes end))
  (ts--save-context
    ;; TODO: Keep mutating the same objects instead of creating a new one each time.
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
            (ts-parse-chunks tree-sitter-parser #'ts-buffer-input tree-sitter-tree)))
    (run-hook-with-args 'tree-sitter-after-change-functions old-tree)))

(defun tree-sitter--enable ()
  "Enable `tree-sitter' in the current buffer."
  (unless tree-sitter-language
    ;; Determine the language symbol based on `major-mode' .
    (let ((lang-symbol (alist-get major-mode tree-sitter-major-mode-language-alist)))
      (unless lang-symbol
        ;; TODO: Consider doing nothing if the language is not supported, so
        ;; that we can make this a global mode.
        (error "No language registered for major mode `%s'" major-mode))
      (setq tree-sitter-language (tree-sitter-require lang-symbol))))
  (unless tree-sitter-parser
    (setq tree-sitter-parser (ts-make-parser))
    (ts-set-language tree-sitter-parser tree-sitter-language))
  (unless tree-sitter-tree
    (tree-sitter--do-parse))
  (add-hook 'before-change-functions #'tree-sitter--before-change :append :local)
  (add-hook 'after-change-functions #'tree-sitter--after-change :append :local))

(defun tree-sitter--disable ()
  "Disable `tree-sitter' in the current buffer."
  (remove-hook 'after-change-functions #'tree-sitter--after-change :local)
  (remove-hook 'before-change-functions #'tree-sitter--before-change :local)
  (setq tree-sitter-tree nil
        tree-sitter-parser nil
        tree-sitter-language nil))

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

(defun tree-sitter-node-at-point ()
  "Return the syntax node at point."
  (let ((root (ts-root-node tree-sitter-tree))
        (p (point)))
    (ts-get-descendant-for-position-range root p p)))

(provide 'tree-sitter)
;;; tree-sitter.el ends here
