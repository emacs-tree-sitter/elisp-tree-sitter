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
      (message "(%s %s %s) -> (%s %s)" byte row column start end)
      (buffer-substring-no-properties start end))))

(defun tree-sitter-pprint (tree)
  (pp (read (tree-sitter-dyn-tree-to-sexp tree))))

(ert-deftest create-parser ()
  (message "%s" (tree-sitter-parser "rust")))

(ert-deftest parse-rust-string ()
  (let ((parser (tree-sitter-parser "rust")))
    (let ((tree (tree-sitter-dyn-parse-string parser "fn foo() {}")))
      (tree-sitter-pprint tree))))

(ert-deftest parse-rust-buffer ()
  (let ((parser (tree-sitter-parser "rust")))
    (with-temp-buffer
      (insert-file-contents
       (concat (file-name-as-directory (getenv "PROJECT_ROOT"))
               "src/lib.rs"))
      (let ((tree (tree-sitter-dyn-parse parser #'tree-sitter-buffer-input)))
        (tree-sitter-pprint tree)))))
