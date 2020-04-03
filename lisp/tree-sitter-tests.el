;;; tree-sitter-tests.el --- Tests for tree-sitter.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; Tests for `tree-sitter'.

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)

(dolist (lang-symbol '(rust bash javascript))
  (tree-sitter-langs-ensure lang-symbol))

(require 'ert)

(defun ts-test-make-parser (lang-symbol)
  "Return a new parser for LANG-SYMBOL."
  (let ((parser (ts-make-parser))
        (language (tree-sitter-require lang-symbol)))
    (ts-set-language parser language)
    parser))

(defun ts-test-full-path (relative-path)
  "Return full path from project RELATIVE-PATH."
  (concat (file-name-directory
           (directory-file-name
            (file-name-directory (locate-library "tree-sitter")))) relative-path))

(defun ts-test-tree-sexp (sexp)
  "Check that the current syntax tree's sexp representation is SEXP."
  (should (equal (read (ts-tree-to-sexp tree-sitter-tree)) sexp)))

(defun ts-test-use-lang (lang-symbol)
  "Turn on `tree-sitter-mode' in the current buffer, using language LANG-SYMBOL."
  (setq tree-sitter-language (tree-sitter-require lang-symbol))
  (tree-sitter-mode))

(defmacro ts-test-with (lang-symbol var &rest body)
  "Eval BODY with VAR bound to a new parser for LANG-SYMBOL."
  (declare (indent 2))
  `(let ((,var (ts-test-make-parser ,lang-symbol)))
     ,@body))

(defmacro ts-test-with-file (relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (ts-test-full-path ,relative-path))
     ,@body))

(defmacro ts-test-lang-with-file (lang-symbol relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH.
`tree-sitter-mode' is turned on, using the given language LANG-SYMBOL."
  (declare (indent 2))
  `(ts-test-with-file ,relative-path
     (ts-test-use-lang ,lang-symbol)
     ,@body))

(ert-deftest creating-parser ()
  (should (ts-parser-p (ts-test-make-parser 'rust))))

(ert-deftest language::equality ()
  (ts-test-with 'rust parser
    ;; XXX: `equal' seems to return nil even if 2 `user-ptr' objects have the same pointer and
    ;; finalizer. That's broken. Report a bug to emacs-devel.
    (should (equal (format "%s" (ts-parser-language parser))
                   (format "%s" (tree-sitter-require 'rust))))))

(ert-deftest language::node-types ()
  (let* ((language (tree-sitter-require 'rust))
         (type-count (ts-lang-count-types language)))
    (ert-info ("0 should be the special node type \"end\"")
      (should (equal "end" (ts-type-name-for-id language 0))))
    (ert-info ("Node type IDs should be from 0 to type count minus 1")
      (should-not (null (ts-type-name-for-id language 1)))
      (should-not (null (ts-type-name-for-id language (- type-count 1))))
      (should (null (ts-type-name-for-id language type-count))))))

(ert-deftest language::fields ()
  (let* ((language (tree-sitter-require 'rust))
         (field-count (ts-lang-count-fields language)))
    (ert-info ("Field IDs should be from 1 to field count")
      (should (null (ts-field-name-for-id language 0)))
      (should-not (null (ts-field-name-for-id language 1)))
      (should-not (null (ts-field-name-for-id language field-count)))
      (should (null (ts-field-name-for-id language (1+ field-count)))))))

(ert-deftest parsing::rust-string ()
  (ts-test-with 'rust parser
    (let ((tree (ts-parse-string parser "fn foo() {}")))
      (should (equal (read (ts-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        name: (identifier)
                        parameters: (parameters)
                        body: (block))))))))

(ert-deftest parsing::without-setting-language ()
  (ert-skip "Need to distinguish between this and timeout/cancellation")
  (let ((parser (ts-make-parser)))
    (should-error (ts-parse-string parser "fn foo() {}") :type 'rust-panic)))

(ert-deftest parsing::rust-buffer ()
  (ts-test-with 'rust parser
    (ts-test-with-file "src/types.rs"
      (let* ((tree) (old-tree)
             (initial (benchmark-run
                          (setq tree (ts-parse-chunks parser #'ts-buffer-input nil))))
             (reparse (benchmark-run
                          (progn
                            (setq old-tree tree)
                            (setq tree (ts-parse-chunks parser #'ts-buffer-input old-tree))))))
        ;; (message "initial %s" initial)
        ;; (message "reparse %s" reparse)
        (ert-info ("Same code should result in empty change ranges")
          (should (equal [] (ts-changed-ranges old-tree tree))))
        (ert-info ("Incremental parsing shoud be faster than initial")
          (should (> (car initial) (car reparse))))))))

(ert-deftest minor-mode::basic-editing ()
  (with-temp-buffer
    (ts-test-use-lang 'rust)
    (ts-test-tree-sexp '(source_file))
    (insert "fn")
    (ts-test-tree-sexp '(source_file (ERROR)))
    (insert " foo() {}")
    (ts-test-tree-sexp '(source_file
                         (function_item
                          name: (identifier)
                          parameters: (parameters)
                          body: (block))))
    (kill-region (point-min) (point-max))
    (ts-test-tree-sexp '(source_file))))

(ert-deftest node::eq ()
  (ts-test-with 'rust parser
    (let* ((tree (ts-parse-string parser "fn foo() {}"))
           (node1 (ts-root-node tree))
           (node2 (ts-root-node tree)))
      (should (ts-node-eq node1 node2))
      (should-not (ts-node-eq node1 (ts-get-nth-child node1 0))))))

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

(ert-deftest cursor::reset ()
  (ts-test-lang-with-file 'rust "src/types.rs"
    (let* ((node (ts-root-node tree-sitter-tree))
           (cursor (ts-make-cursor node)))
      (ts-goto-first-child cursor)
      (should-not (equal (ts-node-type (ts-current-node cursor)) "source_file"))
      (ts-reset-cursor cursor node)
      (should (equal (ts-node-type (ts-current-node cursor)) "source_file")))))

(ert-deftest cursor::using-without-tree ()
  (ts-test-with 'rust parser
    (let ((cursor (ts-make-cursor (ts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (ts-goto-first-child cursor)))
    (garbage-collect)))

(ert-deftest conversion::position<->ts-point ()
  (ts-test-with-file "lisp/tree-sitter-tests.el"
    (ert-info ("Testing buffer boundaries")
      (let ((min (point-min))
            (max (point-max)))
        (should (equal '(1 . 0) (ts-point-from-position min)))
        (should (= min (ts-point-to-position (ts-point-from-position min))))
        (should (= max (ts-point-to-position (ts-point-from-position max))))))
    (ert-info ("Testing arbitrary points")
      (dotimes (_ 100)
        (let ((p (1+ (random (buffer-size)))))
          (should (= p (ts-point-to-position (ts-point-from-position p)))))))))

(ert-deftest buffer-input::non-ascii-characters ()
  (with-temp-buffer
    (insert "\"Tuấn-Anh Nguyễn\";")
    (ts-test-use-lang 'javascript)
    (ts-test-tree-sexp '(program (expression_statement (string))))))

;; https://github.com/ubolonton/emacs-tree-sitter/issues/3
(ert-deftest buffer-input::narrowing ()
  (ts-test-with-file "bin/build"
    (sh-mode)
    (tree-sitter-mode)
    (call-interactively #'mark-whole-buffer)
    (call-interactively #'comment-or-uncomment-region)))

(ert-deftest query::making ()
  (let ((rust (tree-sitter-require 'rust)))
    (ert-info ("Should work on string")
      (should (= (ts-query-count-patterns
                  (ts-make-query rust "(function_item (identifier) @function)
                                       (macro_definition (identifier) @macro)"))
                 2)))
    (ert-info ("Should work on vector")
      (should (= (ts-query-count-patterns
                  (ts-make-query rust [(function_item (identifier) @function)
                                       (macro_definition (identifier) @macro)]))
                 2)))))

(ert-deftest query::basic ()
  (ts-test-lang-with-file 'rust "src/query.rs"
    ;; This is to make sure it works correctly with narrowing.
    (narrow-to-region 1 2)
    (let* ((captures (tree-sitter-query
                      "((function_item (identifier) @function)
                        (match? @function \"make_query\"))
                       (macro_definition (identifier) @macro)"))
           (node-texts (mapcar (lambda (capture)
                                 (pcase-let ((`(_ . ,node) capture))
                                   (ts-node-text node)))
                               captures))
           (capture-names (mapcar (lambda (capture)
                                    (pcase-let ((`(,name . _) capture)) name))
                                  captures)))
      (ert-info ("Should match specified functions and not more")
        (should (member "_make_query" node-texts))
        (should (member "make_query_cursor" node-texts))
        (should (not (member "capture_names" node-texts))))
      (ert-info ("Should capture some macros")
        (should (member "macro" capture-names))))))

(ert-deftest load ()
  (should-error (tree-sitter-require 'abc-xyz))
  (tree-sitter-require 'rust))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
