;;; tree-sitter-extras.el --- Extra functionalities of tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains extra functionalities built on top of `tree-sitter-mode'.
;; They are considered experimental, and subjected to frequent changes.

;;; Code:

(require 'tree-sitter)

(require 'nadvice)

(eval-when-compile
  (require 'subr-x))

(declare-function pixel-posn-y-at-point "ext:pixel-scroll")
(declare-function pixel-scroll-pixel-up "ext:pixel-scroll")
(declare-function pixel-scroll-pixel-down "ext:pixel-scroll")

(defcustom tree-sitter-save-excursion-try-hard nil
  "Whether `tree-sitter-save-excursion' should try as hard as possible."
  :type 'boolean
  :group 'tree-sitter)

(defcustom tree-sitter-save-excursion-pixelwise
  (not (null (require 'pixel-scroll nil :noerror)))
  "Whether `tree-sitter-save-excursion' should restore the location pixelwise."
  :type 'boolean
  :group 'tree-sitter
  :set (lambda (symbol value)
         (when (and value (null (require 'pixel-scroll nil :noerror)))
           (user-error "Pixelwise location restoration requires `pixel-scroll'; you may need to upgrade Emacs"))
         (set-default symbol value)))

(defun tree-sitter--recenter (screen-line &optional pixel-posn-y)
  "Center point on SCREEN-LINE, then optionally scroll to PIXEL-POSN-Y."
  (recenter screen-line)
  (when pixel-posn-y
    (let ((dy (- (pixel-posn-y-at-point) pixel-posn-y)))
      (if (> dy 0)
          (pixel-scroll-pixel-down dy)
        (pixel-scroll-pixel-up (- dy))))))

;;;###autoload
(defmacro tree-sitter-save-excursion (&rest body)
  "Save the current location within the syntax tree; execute BODY; restore it.

If the original location cannot be restored due to the syntax tree changing too
much, this macro behaves like `save-excursion', unless
`tree-sitter-save-excursion-try-hard' is non-nil, in which case it tries to get
as close as possible to the original location.

After the location is restored, the buffer text is scrolled so that point stays
at roughly the same vertical screen position. If `pixel-scroll' is available and
`tree-sitter-save-excursion-pixelwise' is non-nil, pixelwise scrolling is used
instead, to make this restoration exact."
  (declare (indent 0))
  `(let* ((p (point))
          (old-node (tree-sitter-node-at-pos))
          (steps (tsc--node-steps old-node))
          (delta (- p (tsc-node-start-position old-node)))
          (screen-line (- (count-screen-lines (window-start) p) 1))
          (pixel-posn-y ,(if tree-sitter-save-excursion-pixelwise
                             '(pixel-posn-y-at-point)
                           nil)))
     (unwind-protect
         (save-excursion ,@body)
       (condition-case err
           (when-let ((node (tsc--node-from-steps tree-sitter-tree steps)))
             (goto-char (+ delta (tsc-node-start-position node)))
             (tree-sitter--recenter screen-line pixel-posn-y))
         (tsc--invalid-node-step
          ,@(when tree-sitter-save-excursion-try-hard
              '((goto-char (tsc-node-start-position (cadr err)))
                (tree-sitter--recenter screen-line pixel-posn-y))))))))

;;; ----------------------------------------------------------------------------

(declare-function jupyter-repl-cell-code-beginning-position "ext:jupyter-repl")
(declare-function jupyter-repl-cell-code-end-position "ext:jupyter-repl")

(declare-function jupyter-current-client "ext:jupyter-client")
(defvar jupyter-current-client)

(defun tree-sitter--jupyter-repl-set-parse-region-function ()
  (unless (local-variable-p 'tree-sitter-get-parse-region-function)
    (setq-local tree-sitter-get-parse-region-function
                (lambda ()
                  ;; TODO: Return an empty region when we don't have an input
                  ;; cell ready.
                  (save-excursion
                    (save-restriction
                      (widen)
                      (goto-char (point-max))
                      (cons (jupyter-repl-cell-code-beginning-position)
                            (jupyter-repl-cell-code-end-position))))))))

(defun tree-sitter--jupyter-repl-init-cell (&optional type &rest _args)
  (when (and (eq type 'in) tree-sitter-mode)
    (tree-sitter--jupyter-repl-set-parse-region-function)
    ;; FIX: Figure out why Emacs hangs if we parse right now.
    (run-with-idle-timer 0 nil
                         (lambda ()
                           (tree-sitter-resume)))))

(defun tree-sitter--jupyter-repl-finalize-cell (&rest _args)
  (save-excursion
    (goto-char (point-max))
    (when tree-sitter-mode
      (tree-sitter-pause))
    (when tree-sitter-hl-mode
      (tree-sitter-hl-dry-up-region
       (jupyter-repl-cell-code-beginning-position)
       (jupyter-repl-cell-code-end-position)))))

(defun tree-sitter--jupyter-repl-initialize (&optional _arg)
  (when (eq major-mode 'jupyter-repl-mode)
    (unless tree-sitter-language
      (if-let ((lang-symbol (jupyter-kernel-language jupyter-current-client)))
          (with-demoted-errors "tree-sitter--jupyter-repl-initialize: %S"
            (setq tree-sitter-language (tree-sitter-require lang-symbol)))
        (error "Cannot determine jupyter kernel language")))
    (tree-sitter--jupyter-repl-set-parse-region-function)))

;;;###autoload
(defun tree-sitter-enable-jupyter-repl-integration ()
  (advice-add 'jupyter-repl-insert-prompt :after
              #'tree-sitter--jupyter-repl-init-cell)
  (advice-add 'jupyter-repl-finalize-cell :before
              #'tree-sitter--jupyter-repl-finalize-cell)
  (advice-add 'tree-sitter-mode :before
              #'tree-sitter--jupyter-repl-initialize))

;;;###autoload
(defun tree-sitter-disable-jupyter-repl-integration ()
  (advice-remove 'jupyter-repl-insert-prompt
                 #'tree-sitter--jupyter-repl-init-cell)
  (advice-remove 'jupyter-repl-finalize-cell
                 #'tree-sitter--jupyter-repl-finalize-cell)
  (advice-remove 'tree-sitter-mode
                 #'tree-sitter--jupyter-repl-initialize))

;; (cl-defmethod jupyter-repl-after-init (&context (jupyter-lang python))
;;   (tree-sitter-enable 'python)
;;   (cl-call-next-method))

;; (cl-defmethod jupyter-repl-initialize-fontification :after (&context (jupyter-lang python))
;;   (message "HERE HERE HERE")
;;   (tree-sitter-enable 'python)
;;   (tree-sitter-hl-mode))

(provide 'tree-sitter-extras)
;;; tree-sitter-extras.el ends here
