;;; tree-sitter-tests.el --- Tests for tree-sitter.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; Tests for `tree-sitter'.

;;; Code:

(require 'tree-sitter)
(require 'tree-sitter-debug)

(defvar tree-sitter-langs--testing)
;;; Disable grammar downloading.
(let ((tree-sitter-langs--testing t))
  (require 'tree-sitter-langs))
;;; Build the grammars, if necessary.
(dolist (lang-symbol '(rust bash javascript c))
  (tree-sitter-langs-ensure lang-symbol))

(require 'ert)

(eval-when-compile
  (require 'subr-x))

(defun lts-test-make-parser (lang-symbol)
  "Return a new parser for LANG-SYMBOL."
  (let ((parser (lts-make-parser))
        (language (tree-sitter-require lang-symbol)))
    (lts-set-language parser language)
    parser))

(defun lts-test-full-path (relative-path)
  "Return full path from project RELATIVE-PATH."
  (concat (file-name-directory
           (directory-file-name
            (file-name-directory (locate-library "tree-sitter.el")))) relative-path))

(defun lts-test-tree-sexp (sexp &optional reset)
  "Check that the current syntax tree's sexp representation is SEXP.
If RESET is non-nil, also do another full parse and check again."
  (should (equal (read (lts-tree-to-sexp tree-sitter-tree)) sexp))
  (when reset
    (setq tree-sitter-tree nil)
    (tree-sitter--do-parse)
    (lts-test-tree-sexp sexp)))

(defun lts-test-use-lang (lang-symbol)
  "Turn on `tree-sitter-mode' in the current buffer, using language LANG-SYMBOL."
  (setq tree-sitter-language (tree-sitter-require lang-symbol))
  (ignore-errors
    (setq tree-sitter-hl-default-patterns
          (tree-sitter-langs--hl-default-patterns lang-symbol)))
  (add-hook 'tree-sitter-after-first-parse-hook
            (lambda () (should (not (null tree-sitter-tree)))))
  (tree-sitter-mode))

(defmacro lts-test-with (lang-symbol var &rest body)
  "Eval BODY with VAR bound to a new parser for LANG-SYMBOL."
  (declare (indent 2))
  `(let ((,var (lts-test-make-parser ,lang-symbol)))
     ,@body))

(defmacro lts-test-with-file (relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents (lts-test-full-path ,relative-path)))
     ,@body))

(defmacro lts-test-lang-with-file (lang-symbol relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH.
`tree-sitter-mode' is turned on, using the given language LANG-SYMBOL."
  (declare (indent 2))
  `(lts-test-with-file ,relative-path
     (lts-test-use-lang ,lang-symbol)
     ,@body))

(ert-deftest creating-parser ()
  (should (lts-parser-p (lts-test-make-parser 'rust))))

(ert-deftest language::info ()
  (dolist (lang-symbol '(rust bash javascript c))
    (let ((language (tree-sitter-require lang-symbol)))
      (should (eq lang-symbol (lts--lang-symbol language))))))

(ert-deftest language::equality ()
  (lts-test-with 'rust parser
    ;; XXX: `equal' seems to return nil even if 2 `user-ptr' objects have the same pointer and
    ;; finalizer. That's broken. Report a bug to emacs-devel.
    (should (equal (format "%s" (lts-parser-language parser))
                   (format "%s" (tree-sitter-require 'rust))))))

(ert-deftest language::node-types ()
  (let* ((language (tree-sitter-require 'rust))
         (type-count (lts-lang-count-types language)))
    (ert-info ("Round tripping node-type node-type-id")
      (dolist (node-type '(identifier function_item "if" "else"))
        (should (equal (thread-last node-type
                         (lts-lang-node-type-id language)
                         (lts-lang-node-type language))
                       node-type))))
    (ert-info ("0 should be the special node type \"end\"")
      (should (equal 'end (lts-lang-node-type language 0))))
    (ert-info ("Node type IDs should be from 0 to type count minus 1")
      (should-not (null (lts-lang-node-type language 1)))
      (should-not (null (lts-lang-node-type language (- type-count 1))))
      (should (null (lts-lang-node-type language type-count))))))

(ert-deftest language::fields ()
  (let* ((language (tree-sitter-require 'rust))
         (field-count (lts-lang-count-fields language)))
    (ert-info ("Round tripping field field-id")
      (dolist (field '(:name :left :right :value))
        (should (eq (thread-last field
                      (lts-lang-field-id language)
                      (lts-lang-field language))
                    field))))
    (ert-info ("Field IDs should be from 1 to field count")
      (should (null (lts-lang-field language 0)))
      (should (keywordp (lts-lang-field language 1)))
      (should (keywordp (lts-lang-field language field-count)))
      (should (null (lts-lang-field language (1+ field-count)))))))

(ert-deftest parsing::rust-string ()
  (lts-test-with 'rust parser
    (let ((tree (lts-parse-string parser "fn foo() {}")))
      (should (equal (read (lts-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        name: (identifier)
                        parameters: (parameters)
                        body: (block))))))))

(ert-deftest parsing::without-setting-language ()
  (ert-skip "Need to distinguish between this and timeout/cancellation")
  (let ((parser (lts-make-parser)))
    (should-error (lts-parse-string parser "fn foo() {}") :type 'rust-panic)))

(ert-deftest parsing::rust-buffer ()
  (lts-test-with 'rust parser
    (lts-test-with-file "lisp/test-files/types.rs"
      (lts--without-restriction
        (let* ((tree) (old-tree)
               (initial (benchmark-run
                            (setq tree (lts-parse-chunks parser #'lts--buffer-input nil))))
               (reparse (benchmark-run
                            (progn
                              (setq old-tree tree)
                              (setq tree (lts-parse-chunks parser #'lts--buffer-input old-tree))))))
          (ert-info ("Same code should result in empty change ranges")
            (should (equal [] (lts-changed-ranges old-tree tree))))
          (ert-info ("Incremental parsing should be faster than initial")
            (should (> (car initial) (car reparse)))))))))

(ert-deftest minor-mode::basic-editing ()
  (with-temp-buffer
    (lts-test-use-lang 'rust)
    (lts-test-tree-sexp '(source_file))
    (insert "fn")
    (lts-test-tree-sexp '(source_file (ERROR)))
    (insert " foo() {}")
    (lts-test-tree-sexp '(source_file
                         (function_item
                          name: (identifier)
                          parameters: (parameters)
                          body: (block))))
    (kill-region (point-min) (point-max))
    (lts-test-tree-sexp '(source_file))))

(ert-deftest minor-mode::incremental:change-case-region ()
  (lts-test-lang-with-file 'rust "lisp/test-files/change-case-region.rs"
    (let* ((orig-sexp (read (lts-tree-to-sexp tree-sitter-tree)))
           (end (re-search-forward "this text"))
           (beg (match-beginning 0)))
      (upcase-initials-region beg end)
      (lts-test-tree-sexp orig-sexp)
      (downcase-region beg end)
      (lts-test-tree-sexp orig-sexp :reset))))

(ert-deftest minor-mode::incremental:delete-non-ascii-text ()
  (lts-test-lang-with-file 'rust "lisp/test-files/delete-non-ascii-text.rs"
    (let* ((orig-sexp (read (lts-tree-to-sexp tree-sitter-tree)))
           (end (re-search-forward "ấấấấấấấấ"))
           (beg (match-beginning 0)))
      (delete-region beg end)
      (lts-test-tree-sexp orig-sexp :reset))))

(ert-deftest minor-mode::node-at-point ()
  (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
    (should (eq 'source_file (lts-node-type (tree-sitter-node-at-point 'source_file))))
    (search-forward "erase_")
    (should (eq 'identifier (lts-node-type (tree-sitter-node-at-point))))
    (should (eq 'function_item (lts-node-type (tree-sitter-node-at-point 'function_item))))
    (should (null (tree-sitter-node-at-point "function_item")))
    (should (null (tree-sitter-node-at-point 'impl_item)))
    ;; FIX: Signal an error for non-existing node types.
    (should (null (tree-sitter-node-at-point 'non-existing-node-type)))
    (search-forward "struc")
    (should (equal "struct" (lts-node-type (tree-sitter-node-at-point))))
    (should (eq 'struct_item (lts-node-type (tree-sitter-node-at-point 'struct_item))))))

(ert-deftest node::eq ()
  (lts-test-with 'rust parser
    (let* ((tree (lts-parse-string parser "fn foo() {}"))
           (node1 (lts-root-node tree))
           (node2 (lts-root-node tree)))
      (should (lts-node-eq node1 node2))
      (should-not (lts-node-eq node1 (lts-get-nth-child node1 0))))))

(ert-deftest node::using-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (lts-test-with 'rust parser
    (let ((node (lts-root-node
                 (lts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (eql 1 (lts-count-children node))))
    (garbage-collect)))

(ert-deftest node::types ()
  (lts-test-with 'rust parser
    (ert-info ("Error nodes")
      (let* ((root (lts-root-node (lts-parse-string parser "fn")))
             (err (lts-get-nth-child root 0)))
        (should (lts-node-has-error-p root))
        (should-not (lts-node-error-p root))
        (should (eq (lts-node-type root) 'source_file))
        (ert-info ("Should have a special type")
          (should (eq (lts-node-type err) 'ERROR)))
        (should (lts-node-error-p err))
        (should (lts-node-has-error-p err))))
    (ert-info ("Missing nodes")
      (let* ((root (lts-root-node (lts-parse-string parser "let x = 1")))
             (decl (lts-get-nth-child root 0))
             (n (lts-count-children decl))
             (semi (lts-get-nth-child decl (- n 1))))
        (should (lts-node-has-error-p root))
        (should-not (lts-node-error-p root))
        (should (eq (lts-node-type root) 'source_file))
        (ert-info ("Should have a normal type")
          (should (equal (lts-node-type semi) ";")))
        (should (lts-node-missing-p semi))))))

(ert-deftest cursor::walk ()
  (lts-test-with 'rust parser
    (let* ((tree (lts-parse-string parser "fn foo() {}"))
           (node (lts-root-node tree)))
      (ert-info ("Should be able to get a cursor from either a tree or a node")
        (should (lts-cursor-p (lts-make-cursor tree)))
        (should (lts-cursor-p (lts-make-cursor node)))))))

(ert-deftest cursor::reset ()
  (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
    (let* ((node (lts-root-node tree-sitter-tree))
           (cursor (lts-make-cursor node)))
      (lts-goto-first-child cursor)
      (should-not (equal (lts-node-type (lts-current-node cursor)) 'source_file))
      (lts-reset-cursor cursor node)
      (should (equal (lts-node-type (lts-current-node cursor)) 'source_file)))))

(ert-deftest cursor::using-without-tree ()
  (lts-test-with 'rust parser
    (let ((cursor (lts-make-cursor (lts-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (lts-goto-first-child cursor)))
    (garbage-collect)))

(ert-deftest conversion::position<->lts-point ()
  (lts-test-with-file "lisp/tree-sitter-tests.el"
    (ert-info ("Testing buffer boundaries")
      (let ((min (point-min))
            (max (point-max)))
        (should (equal '(1 . 0) (lts-point-from-position min)))
        (should (= min (lts-point-to-position (lts-point-from-position min))))
        (should (= max (lts-point-to-position (lts-point-from-position max))))))
    (ert-info ("Testing arbitrary points")
      (dotimes (_ 100)
        (let ((p (1+ (random (buffer-size)))))
          (should (= p (lts-point-to-position (lts-point-from-position p)))))))))

(ert-deftest buffer-input::non-ascii-characters ()
  (with-temp-buffer
    (insert "\"Tuấn-Anh Nguyễn\";")
    (lts-test-use-lang 'javascript)
    (lts-test-tree-sexp '(program (expression_statement (string))))))

;; https://github.com/ubolonton/emacs-tree-sitter/issues/3
(ert-deftest buffer-input::narrowing ()
  (lts-test-with-file "bin/build"
    (sh-mode)
    (tree-sitter-mode)
    (call-interactively #'mark-whole-buffer)
    (call-interactively #'comment-or-uncomment-region)))

(ert-deftest query::making ()
  (let ((rust (tree-sitter-require 'rust)))
    (ert-info ("Should work on string")
      (should (= (lts-query-count-patterns
                  (lts-make-query rust "(function_item (identifier) @function)
                                       (macro_definition (identifier) @macro)"))
                 2)))
    (ert-info ("Should work on vector")
      (should (= (lts-query-count-patterns
                  (lts-make-query rust [(function_item (identifier) @function)
                                       (macro_definition (identifier) @macro)]))
                 2)))))

(ert-deftest query::basic ()
  (lts-test-lang-with-file 'rust "core/src/query.rs"
    ;; This is to make sure it works correctly with narrowing.
    (narrow-to-region 1 2)
    (let* ((captures (tree-sitter-debug-query
                      "((function_item (identifier) @function)
                        (match? @function \"make_query\"))
                       (macro_definition (identifier) @macro)"))
           (node-texts (mapcar (lambda (capture)
                                 (pcase-let ((`(_ . ,node) capture))
                                   (lts-node-text node)))
                               captures))
           (capture-tags (mapcar (lambda (capture)
                                    (pcase-let ((`(,tag . _) capture)) tag))
                                  captures)))
      (ert-info ("Should match specified functions and not more")
        (should (member "_make_query" node-texts))
        (should (member "make_query_cursor" node-texts))
        (should (not (member "capture_names" node-texts))))
      (ert-info ("Should capture some macros")
        (should (member 'macro capture-tags))))))

(ert-deftest query::range-restriction ()
  ;; https://github.com/tree-sitter/tree-sitter/issues/685
  (lts-test-lang-with-file 'c "lisp/test-files/range-restriction-and-early-termination.c"
    (let ((cursor (lts-make-query-cursor))
          (query (lts-make-query tree-sitter-language
                                [(call_expression
                                  function: (identifier) @function
                                  arguments: (argument_list (string_literal) @string.arg))
                                 (string_literal) @string]))
          (root-node (lts-root-node tree-sitter-tree))
          (capture-names '(function string.arg string)))
      (ert-info ("Querying without range restriction")
        (should (equal (mapcar #'car (lts-query-captures
                                      query root-node #'lts--buffer-substring-no-properties cursor))
                       capture-names))
        (should (equal (mapcar #'car (lts--query-cursor-captures-1
                                      cursor query root-node #'lts--buffer-substring-no-properties))
                       capture-names)))
      (ert-info ("Querying with range restriction")
        (lts--query-cursor-set-byte-range cursor 1 28)
        (should (equal (mapcar #'car (lts-query-captures
                                      query root-node #'lts--buffer-substring-no-properties cursor))
                       capture-names))
        (should (equal (mapcar #'car (lts--query-cursor-captures-1
                                      cursor query root-node #'lts--buffer-substring-no-properties))
                       capture-names))))))

(ert-deftest load ()
  (should-error (tree-sitter-require 'abc-xyz))
  (tree-sitter-require 'rust))

(ert-deftest hl::extend-region ()
  (lts-test-lang-with-file 'rust "lisp/test-files/extend-region.rs"
    (tree-sitter-hl-mode)
    (let* ((beg (save-excursion
                  (re-search-forward "^abc")
                  (backward-char)
                  (point)))
           (end (1+ beg)))
      (tree-sitter-hl--highlight-region beg end)
      (ert-info ("Highlighting a tiny region")
        (should (memq 'tree-sitter-hl-face:function.macro
                      (get-text-property beg 'face)))))))

(ert-deftest hl::hl-region-vs-query-region ()
  (lts-test-lang-with-file 'javascript "lisp/test-files/hl-region-vs-query-region.js"
    (tree-sitter-hl-mode)
    (let ((tree-sitter-hl--extend-region-limit 16)
          id-end id-beg)
      (save-excursion
        (search-forward "compound_assignment_expr:")
        (backward-char)
        (setq id-end (point))
        (search-backward "compound_assignment_expr")
        (setq id-beg (point)))
      (tree-sitter-hl--highlight-region 1 id-end)
      (ert-info ("Highlighting a region that cuts a pattern in halves")
        (should (memq 'tree-sitter-hl-face:property.definition
                      (get-text-property id-beg 'face)))
        (should (eq (next-single-property-change id-beg 'face)
                    id-end))))))

(ert-deftest hl::face-mapping ()
  (ert-info ("Keywords should not be highlighted if their capture name is disabled")
    (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
      ;; Disable keyword highlighting.
      (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name)
                      (not (string= capture-name "keyword"))))
      (tree-sitter-hl-mode)
      (font-lock-ensure)
      (should (null (get-text-property 1 'face)))
      (ert-info ("Other elements should still be highlighted")
        (should-not (null (next-single-property-change 1 'face))))))
  (ert-info ("Keywords should be highlighted by default")
    (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
      (tree-sitter-hl-mode)
      (font-lock-ensure)
      (should (memq 'tree-sitter-hl-face:keyword (get-text-property 1 'face)))))
  (ert-info ("Keywords should not be highlighted if their capture name is disabled")
    (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
      ;; Disable keyword highlighting.
      (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name)
                      (not (string= capture-name "keyword"))))
      (tree-sitter-hl-mode)
      (font-lock-ensure)
      (should (null (get-text-property 1 'face)))
      (ert-info ("Other elements should still be highlighted")
        (should-not (null (next-single-property-change 1 'face))))))
  (ert-info ("Nothing should be highlighted if all capture names are disabled")
    (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
      (add-function :override (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name) nil))
      (tree-sitter-hl-mode)
      (font-lock-ensure)
      (ert-info ("`face' should be nil for the whole buffer")
        (should (null (get-text-property 1 'face)))
        (should (null (next-single-property-change 1 'face)))))))

(ert-deftest hl::bench ()
  (lts-test-lang-with-file 'rust "lisp/test-files/types.rs"
    (setq tree-sitter-hl-default-patterns (tree-sitter-langs--hl-default-patterns 'rust))
    (require 'rust-mode)
    (rust-mode)
    (font-lock-mode)
    (font-lock-set-defaults)
    (tree-sitter-hl-mode)
    (garbage-collect)
    (message "tree-sitter-hl  1 %s" (benchmark-run (font-lock-ensure)))
    (garbage-collect)
    (message "tree-sitter-hl 10 %s" (benchmark-run 10 (font-lock-ensure)))
    (tree-sitter-hl-mode -1)
    (font-lock-ensure)
    (garbage-collect)
    (message "     font-lock  1 %s" (benchmark-run (font-lock-ensure)))
    (garbage-collect)
    (message "     font-lock 10 %s" (benchmark-run 10 (font-lock-ensure)))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
