;; SPDX-License-Identifier: MIT
(require 'seq)

;; XXX: Using `require' after setting`module-file-suffix' to `.dylib' results in
;; "Cannot open load file: No such file or directory, tsc-dyn".
;;
;; XXX: Using `load' results in an error message with garbled text: "Symbol’s
;; value as variable is void: Ïúíþ".
;;
;; Therefore, we use `module-load' directly.
(defun tsc--try-load-dyn (name)
  (or (featurep 'tsc-dyn)
      (condition-case _
          (module-load name)
        (module-open-failed nil))))

(unwind-protect
    (let ((name "tsc-dyn.dylib"))
      ;; Try directory containing `load-file-name'. Typical case.
      (when load-file-name
        (tsc--try-load-dyn (concat (file-name-directory load-file-name)
                              name)))
      ;; Try working directory (e.g. when invoked by `cask').
      (tsc--try-load-dyn name)
      ;; Fall back to `load-path'.
      (seq-find (lambda (dir)
                  (let ((full-name (concat (file-name-as-directory
                                            (expand-file-name dir))
                                           name)))
                    (tsc--try-load-dyn full-name)))
                load-path))
  (fmakunbound 'tsc--try-load-dyn))

;; Local Variables:
;; no-byte-compile: t
;; End:
