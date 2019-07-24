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
  (save-restriction
    (widen)
    (let* ((start (or (byte-to-position (1+ byte)) (point-min)))
           (end (min (+ start 1024) (point-max))))
      (buffer-substring-no-properties start end))))

(defun ts-pprint (tree)
  (pp (read (ts-tree-to-sexp tree))))

(provide 'tree-sitter)
;;; tree-sitter.el ends here
