(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'tree-sitter-dyn)

(defun tree-sitter-parser (lang)
  (let ((parser (tree-sitter--parser))
        (language (tree-sitter--load-language lang nil nil)))
    (tree-sitter--set-language parser language)
    parser))

(defun tree-sitter-buffer-input (byte row column)
  (save-restriction
    (widen)
    (let* ((start (or (byte-to-position byte) (point-min)))
           (end (min (+ start 512) (point-max))))
      ;; (message "(%s %s %s) -> (%s %s)" byte row column start end)
      (buffer-substring-no-properties start end))))

(defun tree-sitter-pprint (tree)
  (pp (read (tree-sitter-tree-to-sexp tree))))

(ert-deftest creating-parser ()
  (message "%s" (tree-sitter-parser "rust")))

(ert-deftest parsing-rust-string ()
  (let ((parser (tree-sitter-parser "rust")))
    (let ((tree (tree-sitter-parse-string parser "fn foo() {}")))
      (should (equal (read (tree-sitter-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        (identifier)
                        (parameters)
                        (block))))))))

(ert-deftest parsing-without-setting-language ()
  (let ((parser (tree-sitter--parser)))
    (should (null (tree-sitter-parse-string parser "fn foo() {}")))))

(ert-deftest parsing-rust-buffer ()
  (let ((parser (tree-sitter-parser "rust")))
    (with-temp-buffer
      (insert-file-contents
       (concat (file-name-as-directory (getenv "PROJECT_ROOT"))
               "src/lib.rs"))
      (let* ((tree)
             (initial (benchmark-run (setq tree (tree-sitter-parse parser #'tree-sitter-buffer-input nil))))
             (reparse (benchmark-run (tree-sitter-parse parser #'tree-sitter-buffer-input tree))))
        (message "initial %s" initial)
        (message "reparse %s" reparse)
        (should (> (car initial) (car reparse)))))))

(ert-deftest using-node-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (let* ((parser (tree-sitter-parser "rust")))
    (message "timing: %s" (benchmark-run 1 (let ((node (tree-sitter-root-node
                                      (tree-sitter-parse-string parser "fn foo() {}"))))
                           (garbage-collect)
                           (should (eql 1 (tree-sitter-child-count node))))))
    (garbage-collect)))

(ert-deftest walk ()
  (let* ((parser (tree-sitter-parser "rust"))
         (tree (tree-sitter-parse-string parser "fn foo() {}"))
         (node (tree-sitter-root-node tree)))
    (tree-sitter-walk tree)
    (tree-sitter-walk node)
    (message "%s" (tree-sitter-foo "abc"))
    (message "%s" (tree-sitter-foo -123))))
