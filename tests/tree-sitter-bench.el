;;; tree-sitter-bench.el --- Benchmarks for tree-sitter.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2022  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Benchmarks for `tree-sitter'.

;;; Code:

;; Local Variables:
;; no-byte-compile: t
;; End:

(require 'tree-sitter-tests-utils)

(ert-deftest parsing::bench ()
  (tsc-test-with c parser
    (tsc-test-with-file "data/types.rs"
      (let ((n 0))
        (while (<= n 4)
          (let ((tsc--buffer-input-chunk-size (* 1024 (expt 2 n))))
            (garbage-collect)
            (message "tsc-parse-chunks %6d %s" tsc--buffer-input-chunk-size
                     (benchmark-run 10
                       (tsc-parse-chunks parser #'tsc--buffer-input nil)))
            (cl-incf n)))))))

(ert-deftest cursor::bench ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (require 'rust-mode)
    (rust-mode)
    (tree-sitter-mode)
    (let ((props [:named-p :type :start-byte :end-byte]))
      (dolist (n '(1 10 100))
        (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        (garbage-collect)
        (message "%10s %3d %s" :do n
                 (eval `(benchmark-run-compiled ,n
                          (tsc-traverse-do ([named-p type start-byte end-byte] tree-sitter-tree)
                            named-p type start-byte end-byte))))
        (garbage-collect)
        (message "%10s %3d %s" :mapc n
                 (eval `(benchmark-run-compiled ,n
                          (tsc-traverse-mapc
                           tsc-test-no-op
                           tree-sitter-tree
                           ,props))))
        (garbage-collect)
        (message "%10s %3d %s" :iter n
                 (eval `(benchmark-run-compiled ,n
                          (iter-do (_ (tsc-traverse-iter tree-sitter-tree ,props))
                            (tsc-test-no-op)))))
        (garbage-collect)
        (message "%10s %3d %s" :node-mapc n
                 (eval `(benchmark-run-compiled ,n
                          (tsc-traverse-mapc
                           (lambda (node)
                             (tsc-node-named-p node)
                             (tsc-node-type node)
                             (tsc-node-start-byte node)
                             (tsc-node-end-byte node)
                             (tsc-test-no-op))
                           tree-sitter-tree))))
        (garbage-collect)
        (message "%10s %3d %s" :node-iter n
                 (eval `(benchmark-run-compiled ,n
                          (iter-do (node (tsc-traverse-iter tree-sitter-tree))
                            (tsc-node-named-p node)
                            (tsc-node-type node)
                            (tsc-node-start-byte node)
                            (tsc-node-end-byte node)
                            (tsc-test-no-op)))))
        (garbage-collect)
        (message "%10s %3d %s" 'funcall n
                 (eval `(benchmark-run-compiled ,(* 3429 n)
                          (funcall tsc-test-no-op ,props 5))))))))

(ert-deftest hl::bench ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (setq tree-sitter-hl-default-patterns (tree-sitter-langs--hl-default-patterns 'rust))
    (require 'rust-mode)
    (rust-mode)
    (font-lock-mode)
    (font-lock-set-defaults)
    (dolist (n '(1 10 100))
      (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (tree-sitter-hl-mode)
      (garbage-collect)
      (message "tree-sitter-hl %2d %s" n (eval `(benchmark-run-compiled ,n (font-lock-ensure))))
      (tree-sitter-hl-mode -1)
      (font-lock-ensure)
      (garbage-collect)
      (message "     font-lock %2d %s" n (eval `(benchmark-run-compiled ,n (font-lock-ensure)))))))

(ert-deftest debug::bench ()
  (tsc-test-lang-with-file rust "data/types.rs"
    (setq tree-sitter-hl-default-patterns (tree-sitter-langs--hl-default-patterns 'rust))
    (require 'rust-mode)
    (rust-mode)
    (dolist (n '(1 10 100))
      (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (dolist (tree-sitter-debug-traversal-method '(:mapc :iter :do))
        (garbage-collect)
        (message "%10s %3d %s" tree-sitter-debug-traversal-method n
                 (eval `(benchmark-run-compiled ,n
                          (progn (tree-sitter-debug-mode -1)
                                 (tree-sitter-debug-mode)))))))))

(provide 'tree-sitter-bench)
;;; tree-sitter-bench.el ends here
