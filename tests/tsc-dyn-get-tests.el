;;; tsc-dyn-get-tests.el --- Tests for tsc-dyn-get.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Tests for the code that downloads/builds and loads `tsc-dyn'. Since Emacs
;; cannot unload dynamic modules, each test needs to run in a fresh Emacs
;; process. These are considered expensive "integration tests" that should be
;; run once in a while, not during normal development.

(require 'async)

(eval-when-compile
  (require 'subr-x))

(defun -remove-tsc-from-load-path ()
  "Remove directories containing `tsc' so that we can properly test.
This is mainly needed because we use `tree-sitter''s env, which depends on
`tsc'."
  (dolist (lib '("tsc" "tsc-dyn"))
    (while
        (when-let* ((loc (locate-library lib))
                    (extra-dir (file-name-directory loc)))
          (setq load-path (delete (file-name-as-directory extra-dir) load-path))
          (setq load-path (delete (directory-file-name extra-dir) load-path))))))

(defvar *root-dir*
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (locate-library "tsc-dyn-get-tests.el")))))

(defmacro -with-sub-eval (&rest body)
  (declare (indent 0))
  `(async-sandbox
    (lambda ()
      (add-to-list 'load-path (concat ,*root-dir* "tests"))
      (load "tsc-dyn-get-tests.el")
      (-remove-tsc-from-load-path)
      ,@body)))

(defmacro -sub-funcall (sym &rest args)
  (declare (indent 1))
  `(-with-sub-eval
     (funcall ',sym ,@args)))

(defmacro -with-fresh-tsc (&rest body)
  `(let ((default-directory (file-name-as-directory
                             (make-temp-file "tsc-dyn-get-tests." :dir))))
     (message "Fresh copy of `tsc' in %s" default-directory)
     (dolist (file '("tsc.el" "tsc-dyn-get.el" "tsc-obsolete.el"
                     "src" "Cargo.toml" "Cargo.lock" ".cargo"))
       (let ((path (concat (file-name-as-directory
                            (concat *root-dir* "core"))
                           file)))
         (if (file-directory-p path)
             (copy-directory path file)
           (copy-file path file))))
     (prog1
         (eval (append
                `(-with-sub-eval
                   (add-to-list 'load-path ,default-directory))
                ',body))
       (delete-directory default-directory :recursive))))

(defmacro -with-call-count (sym &rest body)
  (declare (indent 1))
  `(let* ((cnt 0)
          (incr (lambda (&rest _args) (setq cnt (1+ cnt)))))
     (advice-add ,sym :before incr)
     (cons
      (unwind-protect
          ,@body
        (advice-remove ,sym incr))
      cnt)))

(ert-deftest tsc-dyn-get:no-sources ()
  (should-error (-with-fresh-tsc
                  (setq tsc-dyn-get-from nil)
                  (require 'tsc))
                :type 'file-missing))

(ert-deftest tsc-dyn-get:only-github ()
  (should (equal (-with-fresh-tsc
                   (setq tsc-dyn-get-from '(:github))
                   (-with-call-count 'tsc-dyn-get--github
                     (require 'tsc)))
                 '(tsc . 1))))

(ert-deftest tsc-dyn-get:only-compilation ()
  (should (equal (-with-fresh-tsc
                   (setq tsc-dyn-get-from '(:compilation))
                   (-with-call-count 'tsc-dyn-get--build
                     (require 'tsc)))
                 '(tsc . 1))))

;;; Code:
;; Local Variables:
;; no-byte-compile: t
;; End:
