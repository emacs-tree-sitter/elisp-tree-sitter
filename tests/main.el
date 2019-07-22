(require 'subr-x)

(when-let ((module-path (getenv "MODULE_DIR")))
  (add-to-list 'load-path module-path))

(require 'tree-sitter-dyn)

(defun ts-parser (lang)
  (let ((parser (ts--parser))
        (language (ts--load-language lang nil nil)))
    (ts-set-language parser language)
    parser))

(defun ts-buffer-input (byte row column)
  (save-restriction
    (widen)
    (let* ((start (or (byte-to-position byte) (point-min)))
           (end (min (+ start 512) (point-max))))
      ;; (message "(%s %s %s) -> (%s %s)" byte row column start end)
      (buffer-substring-no-properties start end))))

(defun ts-pprint (tree)
  (pp (read (ts-tree-to-sexp tree))))

(ert-deftest creating-parser ()
  (message "%s" (ts-parser "rust")))

(ert-deftest parsing-rust-string ()
  (let ((parser (ts-parser "rust")))
    (let ((tree (ts-parse-string parser "fn foo() {}")))
      (should (equal (read (ts-tree-to-sexp tree))
                     '(source_file
                       (function_item
                        (identifier)
                        (parameters)
                        (block))))))))

(ert-deftest parsing-without-setting-language ()
  (let ((parser (ts--parser)))
    (should (null (ts-parse-string parser "fn foo() {}")))))

(ert-deftest parsing-rust-buffer ()
  (let ((parser (ts-parser "rust")))
    (with-temp-buffer
      (insert-file-contents
       (concat (file-name-as-directory (getenv "PROJECT_ROOT"))
               "src/lib.rs"))
      (let* ((tree)
             (initial (benchmark-run (setq tree (ts-parse parser #'ts-buffer-input nil))))
             (reparse (benchmark-run (ts-parse parser #'ts-buffer-input tree))))
        (message "initial %s" initial)
        (message "reparse %s" reparse)
        (should (> (car initial) (car reparse)))))))

(ert-deftest using-node-without-tree ()
  "Test that a tree's nodes are still usable after no direct reference to the
tree is held (since nodes internally reference the tree)."
  (let* ((parser (ts-parser "rust")))
    (message "timing: %s" (benchmark-run 1 (let ((node (ts-root-node
                                      (ts-parse-string parser "fn foo() {}"))))
                           (garbage-collect)
                           (should (eql 1 (ts-count-children node))))))
    (garbage-collect)))

(ert-deftest walk ()
  (let* ((parser (ts-parser "rust"))
         (tree (ts-parse-string parser "fn foo() {}"))
         (node (ts-root-node tree)))
    (ts-walk tree)
    (ts-walk node)
    (message "%s" (ts-foo "abc"))
    (message "%s" (ts-foo -123))))
