;;; tree-sitter-tests.el --- Tests for tree-sitter.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; Tests for `tree-sitter'.

;;; Code:

(require 'tree-sitter)

(require 'ert)

(eval-when-compile
  (require 'subr-x))

(defun ts-test-make-parser (lang-symbol)
  "Return a new parser for LANG."
  (let ((parser (ts-make-parser))
        (language (ts-require-language lang-symbol)))
    (ts-set-language parser language)
    parser))

(defun ts-test-full-path (relative-path)
  "Return full path from project RELATIVE-PATH."
  (concat (file-name-directory (locate-library "tree-sitter")) relative-path))

(defun ts-test-tree-sexp (sexp)
  (should (equal (read (ts-tree-to-sexp tree-sitter-tree)) sexp)))

(defmacro ts-test-with (lang-symbol var &rest body)
  "Eval BODY with SYM bound to a new parser for language NAME."
  (declare (indent 2))
  `(let ((,var (ts-test-make-parser ,lang-symbol)))
     ,@body))

(defmacro ts-test-with-temp-buffer (relative-path &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (ts-test-full-path ,relative-path))
     ,@body))

(ert-deftest creating-parser ()
  (should (ts-parser-p (ts-test-make-parser 'rust))))

(ert-deftest parsing::rust-string ()
  (ts-test-with 'rust parser
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
  (ts-test-with 'rust parser
    (ts-test-with-temp-buffer "src/types.rs"
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

(ert-deftest minor-mode::basic-editing ()
  (with-temp-buffer
    (setq tree-sitter-language (ts-require-language 'rust))
    (tree-sitter-mode)
    (ts-test-tree-sexp '(source_file))
    (insert "fn")
    (ts-test-tree-sexp '(source_file (ERROR)))
    (insert " foo() {}")
    (ts-test-tree-sexp '(source_file
                         (function_item
                          (identifier)
                          (parameters)
                          (block))))
    (kill-region (point-min) (point-max))
    (ts-test-tree-sexp '(source_file))))

(ert-deftest node::using-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (ts-test-with 'rust parser
    (let ((node (ts-root-node
                 (ts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (eql 1 (ts-count-children node))))
    (garbage-collect)))

(ert-deftest cursor::walk ()
  (ts-test-with 'rust parser
    (let* ((tree (ts-parse-string parser "fn foo() {}"))
           (node (ts-root-node tree)))
      (ert-info ("Should be able to get a cursor from either a tree or a node")
        (should (ts-cursor-p (ts-make-cursor tree)))
        (should (ts-cursor-p (ts-make-cursor node)))))))

(ert-deftest cursor::using-without-tree ()
  (ts-test-with 'rust parser
    (let ((cursor (ts-make-cursor (ts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (ts-goto-first-child cursor)))
    (garbage-collect)))

(ert-deftest conversion::position<->ts-point ()
  (ts-test-with-temp-buffer "tree-sitter-tests.el"
    (ert-info ("Testing buffer boundaries")
      (let ((min (point-min))
            (max (point-max)))
        (should (equal [0 0] (ts-point-from-position min)))
        (should (= min (ts-point-to-position (ts-point-from-position min))))
        (should (= max (ts-point-to-position (ts-point-from-position max))))))
    (ert-info ("Testing arbitrary points")
      (dotimes (_ 100)
        (let ((p (1+ (random (buffer-size)))))
          (should (= p (ts-point-to-position (ts-point-from-position p)))))))))

(ert-deftest conversion::position<->ts-byte ()
  (ts-test-with-temp-buffer "tree-sitter-tests.el"
    ;; Some non-ascii texts to exercise this test:
    ;; Nguyễn Tuấn Anh
    ;; Нгуен Туан Ань
    ;; 阮俊英
    (ert-info ("Testing buffer boundaries")
      (let ((min (point-min))
            (max (point-max)))
        (should (equal 0 (ts-byte-from-position min)))
        (should (> (ts-byte-from-position max) (buffer-size)))
        (should (= min (ts-byte-to-position (ts-byte-from-position min))))
        (should (= max (ts-byte-to-position (ts-byte-from-position max))))))
    (ert-info ("Testing arbitrary points")
      (dotimes (_ 100)
        (let* ((p0 (1+ (random (buffer-size))))
               (p1 (1+ (random (buffer-size))))
               (b0 (ts-byte-from-position p0))
               (b1 (ts-byte-from-position p1)))
          (should (>= (1+ b0) p0))
          (should (= p0 (ts-byte-to-position b0)))
          (ert-info ("Checking substrings")
            (should (equal
                     (buffer-substring-no-properties (min p0 p1) (max p0 p1))
                     (ts-buffer-substring (min b0 b1) (max b0 b1))))))))))

(ert-deftest buffer-input::non-ascii-characters ()
  (with-temp-buffer
    (insert "\"Tuấn-Anh Nguyễn\";")
    (setq tree-sitter-language (ts-require-language 'javascript))
    (tree-sitter-mode)
    (ts-test-tree-sexp '(program (expression_statement (string))))))

;; https://github.com/ubolonton/emacs-tree-sitter/issues/3
(ert-deftest buffer-input::narrowing ()
  (ts-test-with-temp-buffer "bin/build"
    (sh-mode)
    (setq tree-sitter-language (ts-require-language 'bash))
    (tree-sitter-mode)
    (call-interactively #'mark-whole-buffer)
    (call-interactively #'comment-or-uncomment-region)))

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
