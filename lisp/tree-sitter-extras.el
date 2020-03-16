;;; tree-sitter-extras.el --- Extra functionalities of tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains extra functionalities built on top of `tree-sitter-mode'.
;;
;;; Code:

(require 'tree-sitter)

(eval-when-compile
  (require 'subr-x))

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

(defmacro tree-sitter-save-excursion (&rest body)
  "Save the current location within the syntax tree; execute BODY; restore it.

If the original location cannot be restored due to the syntax tree changing too
much, this macro behaves like `save-excursion', unless
`tree-sitter-save-excursion-try-hard' is non-nil, in which case it tries to get
as close as possible to the original location.

After restoration, if `pixel-scroll' is available (Emacs 26), the buffer text is
scrolled so that the cursor stays at the same vertical position, pixelwise. This
can be disabled by setting `tree-sitter-save-excursion-pixelwise' to nil."
  (declare (indent 0))
  `(let* ((p (point))
          (old-node (tree-sitter-node-at-point))
          (steps (ts--node-steps old-node))
          (delta (- p (ts-node-start-position old-node)))
          (screen-line (- (count-screen-lines (window-start) p) 1))
          (pixel-posn-y ,(if tree-sitter-save-excursion-pixelwise
                             '(pixel-posn-y-at-point)
                           nil)))
     (unwind-protect
         (save-excursion ,@body)
       (condition-case err
           (when-let ((node (ts--node-from-steps tree-sitter-tree steps)))
             (goto-char (+ delta (ts-node-start-position node)))
             (tree-sitter--recenter screen-line pixel-posn-y))
         (ts--invalid-node-step
          ,@(when tree-sitter-save-excursion-try-hard
              '((goto-char (ts-node-start-position (cadr err)))
                (tree-sitter--recenter screen-line pixel-posn-y))))))))

(provide 'tree-sitter-extras)
;;; tree-sitter-extras.el ends here
