;;; tree-sitter.el --- Incremental parsing system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; Keywords: parser dynamic-module
;; Homepage: https://github.com/ubolonton/emacs-tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:

;; This is an Emacs binding of tree-sitter (https://tree-sitter.github.io/tree-sitter/).

;;; Code:

(require 'tree-sitter-dyn)

;;; TODO: Don't hard-code read length.
(defun ts-buffer-input (byte _row _column)
  (let* ((max-position (buffer-size))
         ;; Make sure start-byte is positive.
         (start-byte (max 1 (1+ byte)))
         (end-byte (+ 1024 start-byte))
         ;; nil means > max-position, since we already made sure they are positive.
         (start (or (byte-to-position start-byte) max-position))
         (end (or (byte-to-position end-byte) max-position)))
    (buffer-substring-no-properties start end)))

;; TODO: Windows support?
(defun ts-load-language (name &optional file symbol-prefix)
  "Load and return the language NAME from the shared lib FILE.

It is assumed that language's symbol in the shared lib is prefixed with
SYMBOL-PREFIX. If SYMBOL-PREFIX is nil, it is assumed to be \"tree_sitter_\".

If FILE is nil, load from \"~/.tree-sitter/bin/NAME.so\". This is where the
tree-sitter CLI tool stores the generated shared libs."
  (let ((file (or file
                  (expand-file-name (format "~/.tree-sitter/bin/%s.so" name))))
        (symbol-name (format "%s%s"
                             (or symbol-prefix "tree_sitter_")
                             name)))
    (ts--load-language file symbol-name)))

(defun ts-pprint (tree)
  (pp (read (ts-tree-to-sexp tree))))

(provide 'tree-sitter)
;;; tree-sitter.el ends here
