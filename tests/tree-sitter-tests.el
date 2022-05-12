;;; tree-sitter-tests.el --- Tests for tree-sitter.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2021  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for `tree-sitter'.

;;; Code:

(require 'tree-sitter-tests-utils)

;;; ----------------------------------------------------------------------------
;;; Tests.

(ert-deftest creating-parser ()
  (should (tsc-parser-p (tsc-test-make-parser 'rust))))

(ert-deftest language::info ()
  (dolist (lang-symbol '(rust bash javascript c))
    (let ((language (tree-sitter-require lang-symbol)))
      (should (eq lang-symbol (tsc--lang-symbol language))))))

(ert-deftest language::equality ()
  (tsc-test-with rust parser
    ;; XXX: `equal' seems to return nil even if 2 `user-ptr' objects have the same pointer and
    ;; finalizer. That's broken. Report a bug to emacs-devel.
    (should (equal (format "%s" (tsc-parser-language parser))
                   (format "%s" (tree-sitter-require 'rust))))))

(ert-deftest language::node-types ()
  (let* ((language (tree-sitter-require 'rust))
         (type-count (tsc-lang-count-types language)))
    (ert-info ("Round tripping node-type node-type-id")
      (dolist (node-type '(identifier function_item "if" "else"))
        (should (equal (thread-last node-type
                         (tsc-lang-node-type-id language)
                         (tsc-lang-node-type language))
                       node-type))))
    (ert-info ("0 should be the special node type \"end\"")
      (should (equal :end (tsc-lang-node-type language 0))))
    (ert-info ("Node type IDs should be from 0 to type count minus 1")
      (should-not (null (tsc-lang-node-type language 1)))
      (should-not (null (tsc-lang-node-type language (- type-count 1))))
      (should (null (tsc-lang-node-type language type-count))))))

(ert-deftest language::fields ()
  (let* ((language (tree-sitter-require 'rust))
         (field-count (tsc-lang-count-fields language)))
    (ert-info ("Round tripping field field-id")
      (dolist (field '(:name :left :right :value))
        (should (eq (thread-last field
                      (tsc-lang-field-id language)
                      (tsc-lang-field language))
                    field))))
    (ert-info ("Field IDs should be from 1 to field count")
      (should (null (tsc-lang-field language 0)))
      (should (keywordp (tsc-lang-field language 1)))
      (should (keywordp (tsc-lang-field language field-count)))
      (should (null (tsc-lang-field language (1+ field-count)))))))

(ert-deftest parsing::rust-string ()
  (tsc-test-with rust parser
    (let ((tree (tsc-parse-string parser "fn foo() {}")))
      (should (equal (read (tsc-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        name: (identifier)
                        parameters: (parameters)
                        body: (block))))))))

(ert-deftest parsing::without-setting-language ()
  (ert-skip "Need to distinguish between this and timeout/cancellation")
  (let ((parser (tsc-make-parser)))
    (should-error (tsc-parse-string parser "fn foo() {}") :type 'rust-panic)))

(ert-deftest parsing::rust-buffer ()
  (tsc-test-with rust parser
    (tsc-test-with-file "data/types.rs"
      (tsc--without-restriction
        (let* ((tree) (old-tree)
               (initial (benchmark-run
                            (setq tree (tsc-parse-chunks parser #'tsc--buffer-input nil))))
               (reparse (benchmark-run
                            (progn
                              (setq old-tree tree)
                              (setq tree (tsc-parse-chunks parser #'tsc--buffer-input old-tree))))))
          (ert-info ("Same code should result in empty change ranges")
            (should (equal [] (tsc-changed-ranges old-tree tree))))
          (ert-info ("Incremental parsing should be faster than initial")
            (should (> (car initial) (car reparse)))))))))

(ert-deftest minor-mode::basic-editing ()
  (with-temp-buffer
    (tsc-test-use-lang 'rust)
    (tsc-test-tree-sexp '(source_file))
    (insert "fn")
    (tsc-test-tree-sexp '(source_file (ERROR)))
    (insert " foo() {}")
    (tsc-test-tree-sexp '(source_file
                          (function_item
                           name: (identifier)
                           parameters: (parameters)
                           body: (block))))
    (kill-region (point-min) (point-max))
    (tsc-test-tree-sexp '(source_file))))

(ert-deftest minor-mode::incremental:change-case-region ()
  (cl-flet ((assert-same-range
             (_tree _beg-byte old-end-byte new-end-byte
                    _beg-point old-end-point new-end-point &rest _)
             (should (= old-end-byte new-end-byte))
             (should (equal old-end-point new-end-point))))
    (tsc-test-lang-with-file rust "data/change-case-region.rs"
      (tsc-test-with-advice 'tsc-edit-tree :before #'assert-same-range
        (let* ((orig-sexp (read (tsc-tree-to-sexp tree-sitter-tree)))
               (end (re-search-forward "this text"))
               (beg (match-beginning 0)))
          (upcase-initials-region beg end)
          (tsc-test-tree-sexp orig-sexp)
          (upcase-region beg end)
          (tsc-test-tree-sexp orig-sexp)
          (downcase-region beg end)
          (tsc-test-tree-sexp orig-sexp :reset))))))

(ert-deftest minor-mode::incremental:delete-non-ascii-text ()
  (tsc-test-lang-with-file rust "data/delete-non-ascii-text.rs"
    (let* ((orig-sexp (read (tsc-tree-to-sexp tree-sitter-tree)))
           (end (re-search-forward "ấấấấấấấấ"))
           (beg (match-beginning 0)))
      (delete-region beg end)
      (tsc-test-tree-sexp orig-sexp :reset))))

(ert-deftest minor-mode::node-at-pos ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (should (eq 'use_declaration (tsc-node-type (tree-sitter-node-at-pos :named))))
    (should (eq 'source_file (tsc-node-type (tree-sitter-node-at-pos 'source_file))))
    (should (equal "use" (tsc-node-type (tree-sitter-node-at-pos :anonymous))))
    (should (null (tree-sitter-node-at-pos :anonymous (line-end-position))))
    (should (eq 'identifier (tsc-node-type (tree-sitter-node-at-pos nil 370))))
    (search-forward "erase_")
    (should (eq 'identifier (tsc-node-type (tree-sitter-node-at-pos))))
    (should (eq 'function_item (tsc-node-type (tree-sitter-node-at-pos 'function_item))))
    (should-error (tree-sitter-node-at-pos "function_item"))
    (should (null (tree-sitter-node-at-pos 'impl_item)))
    (should (null (tree-sitter-node-at-pos 'non-existing-node-type nil 'ignore-invalid-type)))
    (should-error (tree-sitter-node-at-pos 'non-existing-node-type nil))
    (search-forward "struc")
    (should (equal "struct" (tsc-node-type (tree-sitter-node-at-pos))))
    (should (equal "struct" (tsc-node-type (tree-sitter-node-at-pos :anonymous))))
    (should (eq 'struct_item (tsc-node-type (tree-sitter-node-at-pos :named))))
    (should (eq 'struct_item (tsc-node-type (tree-sitter-node-at-pos 'struct_item))))))

(ert-deftest node::eq ()
  (tsc-test-with rust parser
    (let* ((tree (tsc-parse-string parser "fn foo() {}"))
           (node1 (tsc-root-node tree))
           (node2 (tsc-root-node tree)))
      (should (tsc-node-eq node1 node2))
      (should-not (tsc-node-eq node1 (tsc-get-nth-child node1 0))))))

(ert-deftest node::using-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (tsc-test-with rust parser
    (let ((node (tsc-root-node
                 (tsc-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (eql 1 (tsc-count-children node))))
    (garbage-collect)))

(ert-deftest node::types ()
  (tsc-test-with rust parser
    (ert-info ("Error nodes")
      (let* ((root (tsc-root-node (tsc-parse-string parser "fn")))
             (err (tsc-get-nth-child root 0)))
        (should (tsc-node-has-error-p root))
        (should-not (tsc-node-error-p root))
        (should (eq (tsc-node-type root) 'source_file))
        (ert-info ("Should have a special type")
          (should (eq (tsc-node-type err) 'ERROR)))
        (should (tsc-node-error-p err))
        (should (tsc-node-has-error-p err))))
    (ert-info ("Missing nodes")
      (let* ((root (tsc-root-node (tsc-parse-string parser "let x = 1")))
             (decl (tsc-get-nth-child root 0))
             (n (tsc-count-children decl))
             (semi (tsc-get-nth-child decl (- n 1))))
        (should (tsc-node-has-error-p root))
        (should-not (tsc-node-error-p root))
        (should (eq (tsc-node-type root) 'source_file))
        (ert-info ("Should have a normal type")
          (should (equal (tsc-node-type semi) ";")))
        (should (tsc-node-missing-p semi))))))

(ert-deftest cursor::walk ()
  (tsc-test-with rust parser
    (let* ((tree (tsc-parse-string parser "fn foo() {}"))
           (node (tsc-root-node tree)))
      (ert-info ("Should be able to get a cursor from either a tree or a node")
        (should (tsc-cursor-p (tsc-make-cursor tree)))
        (should (tsc-cursor-p (tsc-make-cursor node)))))))

(ert-deftest cursor::reset ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (let* ((node (tsc-root-node tree-sitter-tree))
           (cursor (tsc-make-cursor node)))
      (tsc-goto-first-child cursor)
      (should-not (equal (tsc-node-type (tsc-current-node cursor)) 'source_file))
      (tsc-reset-cursor cursor node)
      (should (equal (tsc-node-type (tsc-current-node cursor)) 'source_file))
      (should (equal (tsc-current-node cursor :type) 'source_file))
      (should (equal (tsc-current-node cursor [:start-byte :end-byte :type])
                     `[,(point-min) ,(point-max) source_file]))
      (should-error (tsc-current-node cursor :depth))
      (should-error (tsc-current-node cursor [:depth])))))

(ert-deftest cursor::using-without-tree ()
  (tsc-test-with rust parser
    (let ((cursor (tsc-make-cursor (tsc-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (tsc-goto-first-child cursor)))
    (garbage-collect)))

(ert-deftest cursor::traverse:properties ()
  (tsc-test-with rust parser
    (let* ((code "fn foo(x: usize) {}")
           (rendered (string-trim-left "
source_file (1 . 20)
  function_item (1 . 20)
    :name identifier (4 . 7)
    :parameters parameters (7 . 17)
      parameter (8 . 16)
        :pattern identifier (8 . 9)
        :type primitive_type (11 . 16)
    :body block (18 . 20)
"))
           (tree (tsc-parse-string parser code)))
      (ert-info ("Callback-based traversal should work")
        (should
         (string=
          rendered
          (tsc-test-capture-messages
           (tsc-traverse-mapc
            (lambda (props)
              (pcase-let ((`[,type ,named-p ,start-byte ,end-byte ,field ,depth] props))
                (tsc-test-render-node type named-p start-byte end-byte field depth)))
            tree
            [:type :named-p :start-byte :end-byte :field :depth])))))
      (ert-info ("Iterator-based traversal should work")
        (should
         (string=
          rendered
          (tsc-test-capture-messages
           (cl-loop for item
                    iter-by (tsc-traverse-iter
                             tree [:type :named-p :start-byte :end-byte :field :depth])
                    do (pcase-let ((`[,type ,named-p ,start-byte ,end-byte ,field ,depth] item))
                         (tsc-test-render-node type named-p start-byte end-byte field depth)))))))
      (ert-info ("Inline traversal should work")
        (should
         (string=
          rendered
          (tsc-test-capture-messages
           (tsc-traverse-do ([type named-p start-byte end-byte field depth] tree)
             (tsc-test-render-node type named-p start-byte end-byte field depth)))))))))

(ert-deftest cursor::traverse:single-property ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (let ((tree tree-sitter-tree)
          mapc-result
          do-result
          iter-result)
      (ert-info ("Callback-based traversal should work with single property")
        (tsc-traverse-mapc
         (lambda (type)
           (push type mapc-result))
         tree :type))
      (ert-info ("Iterator-based traversal should work with single property")
        (cl-loop for type
                 iter-by (tsc-traverse-iter tree :type)
                 do (push type iter-result)))
      (tsc-traverse-do ([type] tree)
        (push type do-result))
      (ert-info ("All traversal methods should return the same result")
        (should (equal do-result mapc-result))
        (should (equal do-result iter-result))))))

(ert-deftest cursor::traverse:node ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (let ((tree tree-sitter-tree)
          mapc-result
          do-result
          iter-result)
      (ert-info ("Callback-based traversal should work with nodes")
        (tsc-traverse-mapc
         (lambda (node)
           (push (tsc-node-type node) mapc-result))
         tree))
      (ert-info ("Iterator-based traversal should work with nodes")
        (cl-loop for node
                 iter-by (tsc-traverse-iter tree)
                 do (push (tsc-node-type node) iter-result)))
      (tsc-traverse-do ([type] tree)
        (push type do-result))
      (ert-info ("All traversal methods should return the same result")
        (should (equal do-result mapc-result))
        (should (equal do-result iter-result))))))

(ert-deftest conversion::position<->tsc-point ()
  (tsc-test-with-file "tree-sitter-tests.el"
    (ert-info ("Testing buffer boundaries")
      (let ((min (point-min))
            (max (point-max)))
        (should (equal '(1 . 0) (tsc-point-from-position min)))
        (should (= min (tsc-point-to-position (tsc-point-from-position min))))
        (should (= max (tsc-point-to-position (tsc-point-from-position max))))))
    (ert-info ("Testing arbitrary points")
      (dotimes (_ 100)
        (let ((p (1+ (random (buffer-size)))))
          (should (= p (tsc-point-to-position (tsc-point-from-position p)))))))))

(ert-deftest buffer-input::non-ascii-characters ()
  (with-temp-buffer
    (insert "\"Tuấn-Anh Nguyễn\";")
    (tsc-test-use-lang 'javascript)
    (tsc-test-tree-sexp '(program (expression_statement (string))))))

;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/3
(ert-deftest buffer-input::narrowing ()
  (tsc-test-with-file "data/narrowing.bash"
    (sh-mode)
    (tree-sitter-mode)
    (call-interactively #'mark-whole-buffer)
    (call-interactively #'comment-or-uncomment-region)))

(ert-deftest query::making ()
  (let ((rust (tree-sitter-require 'rust)))
    (ert-info ("Should work on string")
      (should (= (tsc-query-count-patterns
                  (tsc-make-query rust "(function_item (identifier) @function)
                                       (macro_definition (identifier) @macro)"))
                 2)))
    (ert-info ("Should work on vector")
      (should (= (tsc-query-count-patterns
                  (tsc-make-query rust [(function_item (identifier) @function)
                                        (macro_definition (identifier) @macro)]))
                 2)))))

(ert-deftest query::basic ()
  (tsc-test-lang-with-file rust "data/query.rs"
    ;; This is to make sure it works correctly with narrowing.
    (narrow-to-region 1 2)
    (let* ((captures (tree-sitter-debug-query
                      "((function_item (identifier) @function)
                        (match? @function \"make_query\"))
                       (macro_definition (identifier) @macro)"))
           (node-texts (mapcar (lambda (capture)
                                 (pcase-let ((`(_ . ,node) capture))
                                   (tsc-node-text node)))
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
  (tsc-test-lang-with-file c "data/range-restriction-and-early-termination.c"
    (let ((cursor (tsc-make-query-cursor))
          (query (tsc-make-query tree-sitter-language
                                 [(call_expression
                                   function: (identifier) @function
                                   arguments: (argument_list (string_literal) @string.arg))
                                  (string_literal) @string]))
          (root-node (tsc-root-node tree-sitter-tree))
          (capture-names '(function string.arg string)))
      (ert-info ("Querying without range restriction")
        (should (equal (mapcar #'car (tsc-query-captures
                                      query root-node #'tsc--buffer-substring-no-properties cursor))
                       capture-names))
        (should (equal (mapcar #'car (tsc--query-cursor-captures-1
                                      cursor query root-node #'tsc--buffer-substring-no-properties))
                       capture-names)))
      (ert-info ("Querying with range restriction")
        (tsc--query-cursor-set-byte-range cursor 1 28)
        (should (equal (mapcar #'car (tsc-query-captures
                                      query root-node #'tsc--buffer-substring-no-properties cursor))
                       capture-names))
        (should (equal (mapcar #'car (tsc--query-cursor-captures-1
                                      cursor query root-node #'tsc--buffer-substring-no-properties))
                       capture-names))))))

(ert-deftest query::validation ()
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/125
  (should (tsc-make-query (tree-sitter-require 'rust)
                          [(unary_expression (identifier)* @variable)])))

(ert-deftest load ()
  (should-error (tree-sitter-require 'abc-xyz))
  (tree-sitter-require 'rust))

;;; ----------------------------------------------------------------------------
;;; Highlighting tests.

(ert-deftest hl::extend-region ()
  (tsc-test-lang-with-file rust "data/extend-region.rs"
    (tree-sitter-hl-mode)
    (let* ((beg (save-excursion
                  (re-search-forward "^abc")
                  (backward-char)
                  (point)))
           (end (1+ beg)))
      (tree-sitter-hl--highlight-region beg end)
      (ert-info ("Highlighting a tiny region")
        (should (tsc--hl-at beg 'tree-sitter-hl-face:function.macro))))))

(ert-deftest hl::hl-region-vs-query-region ()
  (tsc-test-lang-with-file javascript "data/hl-region-vs-query-region.js"
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
        (should (tsc--hl-at id-beg 'tree-sitter-hl-face:property.definition))
        (should (eq (next-single-property-change id-beg 'face)
                    id-end))))))

(ert-deftest hl::face-mapping ()
  (ert-info ("Keywords should not be highlighted if their capture name is disabled")
    (tsc-test-lang-with-file rust "data/types.rs"
      ;; Disable keyword highlighting.
      (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name)
                      (not (string= capture-name "keyword"))))
      (tree-sitter-hl-mode)
      (tsc--hl-ensure)
      (should (null (get-text-property 1 'face)))
      (ert-info ("Other elements should still be highlighted")
        (should-not (null (next-single-property-change 1 'face))))))
  (ert-info ("Keywords should be highlighted by default")
    (tsc-test-lang-with-file rust "data/types.rs"
      (tree-sitter-hl-mode)
      (tsc--hl-ensure)
      (should (tsc--hl-at 1 'tree-sitter-hl-face:keyword))))
  (ert-info ("Keywords should not be highlighted if their capture name is disabled")
    (tsc-test-lang-with-file rust "data/types.rs"
      ;; Disable keyword highlighting.
      (add-function :before-while (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name)
                      (not (string= capture-name "keyword"))))
      (tree-sitter-hl-mode)
      (tsc--hl-ensure)
      (should (null (get-text-property 1 'face)))
      (ert-info ("Other elements should still be highlighted")
        (should-not (null (next-single-property-change 1 'face))))))
  (ert-info ("Nothing should be highlighted if all capture names are disabled")
    (tsc-test-lang-with-file rust "data/types.rs"
      (add-function :override (local 'tree-sitter-hl-face-mapping-function)
                    (lambda (capture-name) nil))
      (tree-sitter-hl-mode)
      (tsc--hl-ensure)
      (ert-info ("`face' should be nil for the whole buffer")
        (should (null (get-text-property 1 'face)))
        (should (null (next-single-property-change 1 'face)))))))

(ert-deftest hl::with-font-lock-mode-disabled ()
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/74
  (with-current-buffer (find-file (tsc-test-full-path "data/hl.py"))
    (tree-sitter-hl-mode)
    (font-lock-mode -1)
    (tsc--hl-ensure)
    (should (tsc--hl-at 6 'tree-sitter-hl-face:function))))

(ert-deftest debug::jump ()
  "Test if the first button takes us to the beginning of the file.
We know it should since it is the `source_file' node."
  (tsc-test-lang-with-file rust "data/types.rs"
    (let ((buf-name (buffer-name)))
      (setq tree-sitter-debug-jump-buttons t)
      (tree-sitter-debug-mode)
      (goto-char (point-max))
      (should (> (point) 0)) ; Test if worthless if the file is empty
      (switch-to-buffer tree-sitter-debug--tree-buffer nil t)
      (tree-sitter-debug--button-node-lookup (button-at 1))
      (should (= (point) (point-min))))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'tree-sitter-tests)
;;; tree-sitter-tests.el ends here
