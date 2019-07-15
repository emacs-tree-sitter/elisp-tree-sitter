(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'tree-sitter-dyn)

(defun tree-sitter-parser (lang)
  (let ((parser (tree-sitter-dyn--parser))
        (language (tree-sitter-dyn--load-language lang)))
    (tree-sitter-dyn--set-language parser language)
    parser))

(defun tree-sitter-buffer-input (byte row column)
  (save-restriction
    (widen)
    (let* ((start (or (byte-to-position byte) (point-min)))
           (end (min (+ start 512) (point-max))))
      ;; (message "(%s %s %s) -> (%s %s)" byte row column start end)
      (buffer-substring-no-properties start end))))

(defun tree-sitter-pprint (tree)
  (pp (read (tree-sitter-dyn-tree-to-sexp tree))))

(ert-deftest creating-parser ()
  (message "%s" (tree-sitter-parser "rust")))

(ert-deftest parsing-rust-string ()
  (let ((parser (tree-sitter-parser "rust")))
    (let ((tree (tree-sitter-dyn-parse-string parser "fn foo() {}")))
      (should (equal (read (tree-sitter-dyn-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        (identifier)
                        (parameters)
                        (block))))))))

(ert-deftest parsing-without-setting-language ()
  (let ((parser (tree-sitter-dyn--parser)))
    (should (null (tree-sitter-dyn-parse-string parser "fn foo() {}")))))

(ert-deftest parsing-rust-buffer ()
  (let ((parser (tree-sitter-parser "rust")))
    (with-temp-buffer
      (insert-file-contents
       (concat (file-name-as-directory (getenv "PROJECT_ROOT"))
               "src/lib.rs"))
      (let* ((tree)
             (initial (benchmark-run (setq tree (tree-sitter-dyn-parse parser #'tree-sitter-buffer-input nil))))
             (reparse (benchmark-run (tree-sitter-dyn-parse parser #'tree-sitter-buffer-input tree))))
        (message "initial %s" initial)
        (message "reparse %s" reparse)
        (should (> (car initial) (car reparse)))))))

(ert-deftest using-node-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (let* ((parser (tree-sitter-parser "rust")))
    (let ((node (tree-sitter-dyn-root-node
                 (tree-sitter-dyn-parse-string parser "fn foo() {}"))))
      (garbage-collect)
      (should (eql 1 (tree-sitter-dyn-child-count node))))
    (garbage-collect)))
