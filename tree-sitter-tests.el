;;; tree-sitter.el --- Incremental parsing system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; Tests for `tree-sitter'.

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
  (concat (file-name-directory (locate-library "tree-sitter")) relative-path))

(defmacro ts-test-with (name sym &rest body)
  "Eval BODY with SYM bound to a new parser for language NAME."
  (declare (indent 2))
  `(let ((,sym (ts-test-make-parser ,name)))
     ,@body))

(ert-deftest creating-parser ()
  (should (ts-parser-p (ts-test-make-parser "rust"))))

(ert-deftest parsing::rust-string ()
  (ts-test-with "rust" parser
    (let ((tree (ts-parse-string parser "fn foo() {}")))
      (should (equal (read (ts-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        (identifier)
                        (parameters)
                        (block))))))))

(ert-deftest parsing::without-setting-language ()
  (ert-skip "Need to distinguish between this and timeout/cancellation")
  (let ((parser (ts-make-parser)))
    (should-error (ts-parse-string parser "fn foo() {}") :type 'rust-panic)))

(ert-deftest parsing::rust-buffer ()
  (ts-test-with "rust" parser
    (with-temp-buffer
      (insert-file-contents (ts-test-full-path "src/types.rs"))
      (let* ((tree) (old-tree)
             (initial (benchmark-run
                          (setq tree (ts-parse parser #'ts-buffer-input nil))))
             (reparse (benchmark-run
                          (progn
                            (setq old-tree tree)
                            (setq tree (ts-parse parser #'ts-buffer-input old-tree))))))
        ;; (message "initial %s" initial)
        ;; (message "reparse %s" reparse)
        (ert-info ("Same code should result in empty change ranges")
          (should (equal [] (ts-changed-ranges tree old-tree))))
        (ert-info ("Incremental parsing shoud be faster than initial")
          (should (> (car initial) (car reparse))))))))

(ert-deftest node::using-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (ts-test-with "rust" parser
    (let ((node (ts-root-node
                 (ts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (eql 1 (ts-count-children node))))
    (garbage-collect)))

(ert-deftest cursor::walk ()
  (ts-test-with "rust" parser
    (let* ((tree (ts-parse-string parser "fn foo() {}"))
           (node (ts-root-node tree)))
      (ert-info ("Should be able to get a cursor from either a tree or a node")
        (should (ts-cursor-p (ts-make-cursor tree)))
        (should (ts-cursor-p (ts-make-cursor node)))))))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
