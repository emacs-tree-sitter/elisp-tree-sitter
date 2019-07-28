;;; tree-sitter-core.el --- Core tree-sitter APIs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the core functionalities of tree-sitter.
;;
;;; Code:

(require 'tree-sitter-dyn)
(require 'pp)

(defun ts-buffer-input (byte _row _column)
  "Return current buffer's text starting from the given (0-based indexed) BYTE."
  (let* ((max-position (1+ (buffer-size)))
         ;; Make sure start-byte is positive.
         (start-byte (max 1 (1+ byte)))
         ;; TODO: Don't hard-code read length.
         (end-byte (+ 1024 start-byte))
         ;; nil means > max-position, since we already made sure they are positive.
         (start (or (byte-to-position start-byte) max-position))
         (end (or (byte-to-position end-byte) max-position)))
    ;; (message "%s [%s %s] -> [%s %s] %s" byte _row _column start-byte end-byte (- end-byte start-byte))
    (buffer-substring-no-properties start end)))

;;; TODO: Keep a global (symbol -> language) registry.
(defun ts-load-language (name &optional file symbol-prefix)
  "Load and return the language NAME from the shared lib FILE.

It is assumed that language's symbol in the shared lib is prefixed with
SYMBOL-PREFIX. If SYMBOL-PREFIX is nil, it is assumed to be \"tree_sitter_\".

If FILE is nil, load from \"~/.tree-sitter/bin/NAME.so\". This is where the
tree-sitter CLI tool stores the generated shared libs."
  (let* ((ext (pcase system-type
                ((or 'darwin 'gnu/linux) "so")
                ('windows-nt "dll")
                (_ (error "Unsupported system-type %s" system-type))))
         (file (or file
                   (expand-file-name (format "~/.tree-sitter/bin/%s.%s" name ext))))
         (symbol-name (format "%s%s"
                              (or symbol-prefix "tree_sitter_")
                              name)))
    (message "Loading '%s'" file)
    (ts--load-language file symbol-name)))

(defun ts-pp-to-string (tree)
  "Return the pretty-printed string of TREE's sexp."
  (pp-to-string (read (ts-tree-to-sexp tree))))

(provide 'tree-sitter-core)
;;; tree-sitter-core.el ends here
