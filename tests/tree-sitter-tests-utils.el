;;; tree-sitter-tests-utils.el --- Utils for tree-sitter-tests.el -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2022  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Utils for `tree-sitter-tests'.

;;; Code:

(setq tsc-dyn-get-from nil)
(require 'tree-sitter)
(require 'tree-sitter-debug)

(defvar tree-sitter-langs--testing)
;;; Disable grammar downloading.
(let ((tree-sitter-langs--testing t))
  (require 'tree-sitter-langs))
;;; Build the grammars, if necessary.
(dolist (lang-symbol '(rust python javascript c))
  (tree-sitter-langs-ensure lang-symbol))

;; XXX: Bash grammar failed 'tree-sitter test' on Windows: 'Escaped newlines'.
(with-demoted-errors "Failed to ensure bash grammar %s"
  (tree-sitter-langs-ensure 'bash))

(require 'ert)
(require 'generator)

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

;;; ----------------------------------------------------------------------------
;;; Helpers.

(defun tsc-test-make-parser (lang-symbol)
  "Return a new parser for LANG-SYMBOL."
  (let ((parser (tsc-make-parser))
        (language (tree-sitter-require lang-symbol)))
    (tsc-set-language parser language)
    parser))

(defun tsc-test-full-path (relative-path)
  "Return full path from project RELATIVE-PATH."
  (concat (file-name-directory (locate-library "tree-sitter-tests.el"))
          relative-path))

(defun tsc-test-tree-sexp (sexp &optional reset)
  "Check that the current syntax tree's sexp representation is SEXP.
If RESET is non-nil, also do another full parse and check again."
  (should (equal (read (tsc-tree-to-sexp tree-sitter-tree)) sexp))
  (when reset
    (setq tree-sitter-tree nil)
    (tree-sitter--do-parse)
    (tsc-test-tree-sexp sexp)))

(defun tsc-test-use-lang (lang-symbol)
  "Turn on `tree-sitter-mode' in the current buffer, using language LANG-SYMBOL."
  (setq tree-sitter-language (tree-sitter-require lang-symbol))
  (ignore-errors
    (setq tree-sitter-hl-default-patterns
          (tree-sitter-langs--hl-default-patterns lang-symbol)))
  (add-hook 'tree-sitter-after-first-parse-hook
            (lambda () (should (not (null tree-sitter-tree)))))
  (tree-sitter-mode))

(defun tsc--listify (x)
  (if (listp x)
      x
    (list x)))

(defun tsc--hl-at (pos face)
  "Return t if text at POS is highlighted with FACE."
  (memq face (tsc--listify (get-text-property pos 'face))))

;; In Emacs 28,`font-lock-ensure' checks `font-lock-specified-p' first.
;; See https://github.com/emacs-tree-sitter/elisp-tree-sitter/pull/220#issuecomment-1120423580.
(defun tsc--hl-ensure (&optional beg end)
  (funcall font-lock-ensure-function
           (or beg (point-min)) (or end (point-max))))

(defun tsc-test-no-op (&rest _args))

(defvar tsc-test-no-op
  (byte-compile #'tsc-test-no-op))

(defun tsc-test-render-node (type named-p start-byte end-byte field depth)
  (when named-p
    (message "%s%s%S (%s . %s)" (make-string (* 2 depth) ?\ )
             (if field
                 (format "%s " field)
               "")
             type start-byte end-byte)))

(defmacro tsc-test-with (lang-symbol var &rest body)
  "Eval BODY with VAR bound to a new parser for LANG-SYMBOL."
  (declare (indent 2))
  `(let ((,var (tsc-test-make-parser ',lang-symbol)))
     ,@body))

(defmacro tsc-test-with-file (relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((coding-system-for-read 'utf-8))
       (insert-file-contents (tsc-test-full-path ,relative-path)))
     ,@body))

(defmacro tsc-test-lang-with-file (lang-symbol relative-path &rest body)
  "Eval BODY in a temp buffer filled with content of the file at RELATIVE-PATH.
`tree-sitter-mode' is turned on, using the given language LANG-SYMBOL."
  (declare (indent 2))
  `(tsc-test-with-file ,relative-path
     (tsc-test-use-lang ',lang-symbol)
     ,@body))

(defmacro tsc-test-with-advice (symbol where function &rest body)
  "Eval BODY while advising SYMBOL with FUNCTION at WHERE."
  (declare (indent 3))
  `(progn
     (advice-add ,symbol ,where ,function)
     (unwind-protect
         ,@body
       (advice-remove ,symbol ,function))))

(defmacro tsc-test-capture-messages (&rest body)
  `(with-temp-buffer
     (let ((buf (current-buffer)))
       (tsc-test-with-advice 'message :override
                             (lambda (fmt &rest args)
                               (with-current-buffer buf
                                 (insert (apply #'format-message fmt args) "\n")))
         ,@body)
       (with-current-buffer buf
         (buffer-string)))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'tree-sitter-tests-utils)
;;; tree-sitter-tests-utils.el ends here
