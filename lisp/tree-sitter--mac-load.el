(require 'seq)

;; XXX: Using `require' after setting`module-file-suffix' to `.dylib' results in "Cannot open load
;; file: No such file or directory, tree-sitter-dyn".
;;
;; XXX: Using `load' results in an error message with garbled text: "Symbol’s value as variable is
;; void: Ïúíþ".
;;
;; Therefore, we use `module-load' directly.
(defun ts--try-load-dyn (name)
  (or (featurep 'tree-sitter-dyn)
      (condition-case _
          (module-load name)
        (module-open-failed nil))))

(unwind-protect
    (let ((name "tree-sitter-dyn.dylib"))
      ;; Try directory containing `load-file-name'. Typical case.
      (when load-file-name
        (ts--try-load-dyn (concat (file-name-directory load-file-name)
                              name)))
      ;; Try working directory (e.g. when invoked by `cask').
      (ts--try-load-dyn name)
      ;; Fall back to `load-path'.
      (seq-find (lambda (dir)
                  (let ((full-name (concat (file-name-as-directory
                                            (expand-file-name dir))
                                           name)))
                    (ts--try-load-dyn full-name)))
                load-path))
  (fmakunbound 'ts--try-load-dyn))
