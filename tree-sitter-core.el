;;; tree-sitter-core.el --- Core tree-sitter APIs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the core functionalities of tree-sitter.
;;
;;; Code:

(require 'tree-sitter-dyn)

(require 'simple)
(require 'map)
(require 'pp)

(eval-when-compile
  (require 'subr-x))

(defsubst ts-byte-from-position (position)
  "Return tree-sitter (0-based) byte offset for character at POSITION."
  (- (position-bytes position) 1))

(defsubst ts-byte-to-position (byte)
  "Return the character position for tree-sitter (0-based) BYTE offset."
  (byte-to-position (1+ byte)))

(defun ts-point-from-position (position)
  "Convert POSITION to a valid (0-based indexed) tree-sitter point.
The returned column counts bytes, which is different from `current-column'.
Narrowing must be removed before calling this function, using `save-restriction'
and `widen'."
  (save-excursion
    (goto-char position)
    (let ((row (- (line-number-at-pos position) 1))
          ;; TODO: Add tests that fail if `current-column' is used instead.
          (column (- (position-bytes position)
                     (position-bytes (line-beginning-position)))))
      (vector row column))))

(defun ts-point-to-position (point)
  "Convert tree-sitter POINT to buffer position.
Narrowing must be removed before calling this function, using `save-restriction'
and `widen'."
  (save-excursion
    (let ((row (aref point 0))
          (column (aref point 1)))
      (goto-char 1)
      (forward-line row)
      (ts-byte-to-position (+ column (ts-byte-from-position (line-beginning-position)))))))

(defsubst ts-buffer-substring (beg-byte end-byte)
  "Return the current buffer's text between (0-based) BEG-BYTE and END-BYTE.
Narrowing must be removed before calling this function, using `save-restriction'
and `widen'."
  (buffer-substring-no-properties
   (ts-byte-to-position beg-byte)
   (ts-byte-to-position end-byte)))

(defun ts-buffer-input (byte _row _column)
  "Return a portion of the current buffer's text starting from the given (0-based) BYTE offset.
BYTE is automatically clamped to the valid range.

Narrowing must be removed before calling this function, using `save-restriction'
and `widen'."
  (let* ((max-position (point-max))
         (beg-byte (max 0 byte))
         ;; ;; TODO: Don't hard-code read length.
         (end-byte (+ 1024 beg-byte))
         ;; nil means > max-position, since we already made sure they are non-negative.
         (start (or (ts-byte-to-position beg-byte) max-position))
         (end (or (ts-byte-to-position end-byte) max-position)))
    (buffer-substring-no-properties start end)))

(defun ts--get-cli-directory ()
  "Return tree-sitter CLI's directory, including the ending separator.
This is the directory where the CLI tool keeps compiled lang definitions, among
other data."
  (file-name-as-directory
   (expand-file-name
    ;; https://github.com/tree-sitter/tree-sitter/blob/1bad6dc/cli/src/config.rs#L20
    (if-let ((dir (getenv "TREE_SITTER_DIR")))
        dir
      "~/.tree-sitter"))))

(defvar ts--languages nil
  "An alist of mappings from language name symbols to language objects.
See `ts-require-language'.")

(defun ts--load-language-from-cli-dir (name &optional noerror)
  "Load and return the language NAME from the tree-sitter CLI's dir.
See `ts--get-cli-directory'.

If optional arg NOERROR is non-nil, report no error if loading fails."
  (let* ((ext (pcase system-type
                ((or 'darwin 'gnu/linux) "so")
                ('windows-nt "dll")
                (_ (error "Unsupported system-type %s" system-type))))
         (file (concat (file-name-as-directory
                        (concat (ts--get-cli-directory) "bin"))
                       (format "%s.%s" name ext)))
         (symbol-name (format "tree_sitter_%s" name)))
    (if noerror
        (condition-case error
            (ts--load-language file symbol-name)
          (rust-error nil))
      (ts--load-language file symbol-name))))

;;; TODO: Support more loading mechanisms: bundled statically, packaged-together shared libs, shared
;;; libs under ~/.emacs.d/.tree-sitter
(defun ts-load-language (lang-symbol)
  "Load and return a new language object identified by LANG-SYMBOL.
The language should have been installed using tree-sitter CLI."
  (ts--load-language-from-cli-dir (symbol-name lang-symbol)))

(defun ts-require-language (lang-symbol)
  "Return the language object identified by LANG-SYMBOL.
If the language hasn't been loaded yet, this function attempts to load it."
  (let ((language (alist-get lang-symbol ts--languages)))
    (unless language
      (setq language (ts-load-language lang-symbol))
      (map-put ts--languages lang-symbol language))
    language))

(defun ts-pp-to-string (tree)
  "Return the pretty-printed string of TREE's sexp."
  (pp-to-string (read (ts-tree-to-sexp tree))))

(provide 'tree-sitter-core)
;;; tree-sitter-core.el ends here
