;;; tree-sitter.el --- Incremental parsing system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; Tests for tree-sitter.

;;; Code:

(require 'subr-x)

(require 'tree-sitter)

(defun ts-test-make-parser (lang)
  "Return a new parser for LANG."
  (let ((parser (ts-make-parser))
        (language (ts-load-language lang)))
    (ts-set-language parser language)
    parser))

(defun ts-test-full-path (relative-path)
  "Return full path from project RELATIVE-PATH."
  (concat (file-name-as-directory (getenv "PROJECT_ROOT")) relative-path))

(ert-deftest creating-parser ()
  (should (ts-parser-p (ts-test-make-parser "rust"))))

(ert-deftest parsing-rust-string ()
  (let ((parser (ts-test-make-parser "rust")))
    (let ((tree (ts-parse-string parser "fn foo() {}")))
      (should (equal (read (ts-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        (identifier)
                        (parameters)
                        (block))))))))

(ert-deftest parsing-without-setting-language ()
  (let ((parser (ts-make-parser)))
    (should (null (ts-parse-string parser "fn foo() {}")))))

(ert-deftest parsing-rust-buffer ()
  (let ((parser (ts-test-make-parser "rust")))
    (with-temp-buffer
      (insert-file-contents (ts-test-full-path "src/types.rs"))
      (let* ((tree)
             (initial (benchmark-run (setq tree (ts-parse parser #'ts-buffer-input nil))))
             (reparse (benchmark-run (ts-parse parser #'ts-buffer-input tree))))
        (message "initial %s" initial)
        (message "reparse %s" reparse)
        (should (> (car initial) (car reparse)))))))

(ert-deftest using-node-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (let* ((parser (ts-test-make-parser "rust")))
    (message "timing: %s" (benchmark-run 1 (let ((node (ts-root-node
                                      (ts-parse-string parser "fn foo() {}"))))
                           (garbage-collect)
                           (should (eql 1 (ts-count-children node))))))
    (garbage-collect)))

(ert-deftest walk ()
  (let* ((parser (ts-test-make-parser "rust"))
         (tree (ts-parse-string parser "fn foo() {}"))
         (node (ts-root-node tree)))
    (should (ts-cursor-p (ts-make-cursor tree)))
    (should (ts-cursor-p (ts-make-cursor node)))))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
