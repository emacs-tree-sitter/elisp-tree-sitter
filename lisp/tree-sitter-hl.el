;;; tree-sitter-hl.el --- Syntax highlighting based on tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;;         Timo von Hartz <c0untlizzi@gmail.com>

;;; Commentary:

;; This file implements a new syntax highlighting based on `tree-sitter'.

;;; Code:

(require 'tree-sitter)

(eval-when-compile
  (require 'cl-lib))

(defgroup tree-sitter-hl nil
  "Syntax highlighting using tree-sitter."
  :group 'tree-sitter)

(defgroup tree-sitter-hl-faces nil
  "All the faces of tree-sitter."
  :group 'tree-sitter-hl)

(defface tree-sitter-attribute-face '((default :inherit font-lock-preprocessor-face))
  "Face used for attribute"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-comment-face '((default :inherit font-lock-comment-face))
  "Face used for comment"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-constant-face '((default :inherit font-lock-constant-face))
  "Face used for constant"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-constant-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for constant.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-constructor-face '((default :inherit font-lock-type-face))
  "Face used for constructor"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-escape-face '(())
  "Face used for escape"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-function-face '((default :inherit font-lock-function-name-face))
  "Face used for function"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-function-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for function.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-function-macro-face '((default :inherit font-lock-preprocessor-face))
  "Face used for function.macro"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-function-method-face '((default :inherit link :underline nil))
  "Face used for function.method"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-identifier-face '((default :inherit font-lock-function-name-face))
  "Face used for identifier"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-keyword-face '((default :inherit font-lock-keyword-face))
  "Face used for keyword"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-label-face '((default :inherit font-lock-preprocessor-face))
  "Face used for label"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-operator-face '((default :inherit font-lock-keyword-face))
  "Face used for operator"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-property-face '((default :inherit font-lock-variable-name-face))
  "Face used for property"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-punctuation-face '(())
  "Face used for punctuation"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-punctuation-bracket-face '(())
  "Face used for punctuation.bracket"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-punctuation-delimiter-face '(())
  "Face used for punctuation.delimiter"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-string-face '((default :inherit font-lock-string-face))
  "Face used for string"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-type-face '((default :inherit font-lock-type-face))
  "Faced used for type"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-type-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for type.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-variable-face '((default :inherit font-lock-variable-name-face))
  "Face used for variable"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-variable-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for variable.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-variable-parameter-face '((default :inherit font-lock-variable-name-face))
  "Faced used for variable.parameter"
  :group 'tree-sitter-hl-faces)

(defcustom tree-sitter-hl-default-faces
  '(("attribute"             . tree-sitter-attribute-face)
    ("comment"               . tree-sitter-comment-face)
    ("constant"              . tree-sitter-constant-face)
    ("constant.builtin"      . tree-sitter-constant-builtin-face)
    ("constructor"           . tree-sitter-constructor-face)
    ("escape"                . tree-sitter-escape-face)
    ("function"              . tree-sitter-function-face)
    ("function.builtin"      . tree-sitter-function-builtin-face)
    ("function.macro"        . tree-sitter-function-macro-face)
    ("function.method"       . tree-sitter-function-method-face)
    ("identifier"            . tree-sitter-identifier-face)
    ("keyword"               . tree-sitter-keyword-face)
    ("label"                 . tree-sitter-label-face)
    ("operator"              . tree-sitter-operator-face)
    ("property"              . tree-sitter-property-face)
    ("punctuation"           . tree-sitter-punctuation-face)
    ("punctuation.bracket"   . tree-sitter-punctuation-bracket-face)
    ("punctuation.delimiter" . tree-sitter-punctuation-delimiter-face)
    ("string"                . tree-sitter-string-face)
    ("type"                  . tree-sitter-type-face)
    ("type.builtin"          . tree-sitter-type-builtin-face)
    ("variable"              . tree-sitter-variable-face)
    ("variable.builtin"      . tree-sitter-variable-builtin-face)
    ("variable.parameter"    . tree-sitter-variable-parameter-face))
  "Alist of query identifier to face used for highlighting matches."
  :type '(alist :key-type string
                :value-type face)
  :group 'tree-sitter-hl)

(defvar-local tree-sitter-hl--query nil)
(defvar-local tree-sitter-hl--query-cursor nil)

(defun tree-sitter-hl--face (capture-name)
  (map-elt tree-sitter-hl-default-faces capture-name nil #'string=))

(defun tree-sitter-hl--append-text-property (start end prop value &optional object)
  "Like `font-lock-append-text-property', but deduplicates values
It also expects VALUE to be a single value, not a list."
  (let (next prev)
    (while (/= start end)
      (setq next (next-single-property-change start prop object end)
            prev (get-text-property start prop object))
      ;; Canonicalize old forms of face property.
      (and (memq prop '(face font-lock-face))
           (listp prev)
           (or (keywordp (car prev))
               (memq (car prev) '(foreground-color background-color)))
           (setq prev (list prev)))
      (unless (listp prev)
        (setq prev (list prev)))
      (unless (memq value prev)
        (put-text-property start next prop
                           (append prev (list value))
                           object))
      (setq start next))))

(defun tree-sitter-hl--highlight-capture (capture)
  (pcase-let* ((`(,name . ,node) capture)
               (face (map-elt tree-sitter-hl-default-faces name nil #'string=))
               (`(,beg . ,end) (ts-node-position-range node)))
    ;; (message " %s <- %s <- [%s %s]" face name beg end)

    ;; TODO: I think it's better to compute faces for each node first, in Rust.
    ;; Additionally, we can give certain combinations of capture names their own
    ;; faces. For example, it might be desirable for fontification of a node
    ;; that matches both "constructor" and "variable" to be different from the
    ;; union of "constructor fontification" and "variable fontification".
    (when face

      ;; Naive `add-face-text-property' keeps adding the same face, slowing
      ;; things down.
      ;; (add-face-text-property beg end face)

      ;; (font-lock-append-text-property beg end 'face face)

      ;; `put-text-property' doesn't handle list of faces.
      ;; (put-text-property beg end 'face face)

      ;; TODO: Deal with faces previously set by us.
      (tree-sitter-hl--append-text-property beg end 'face face))))

;;; TODO: Handle embedded DSLs (injections).
(defun tree-sitter-hl--highlight-region (beg end &optional _loudly)
  (message "tree-sitter-hl [%s %s]" beg end)
  (ts--save-context
    (ts-set-point-range tree-sitter-hl--query-cursor
                        (ts--point-from-position beg)
                        (ts--point-from-position end))
    (let* ((root-node (ts-root-node tree-sitter-tree))
           (captures (ts-query-captures
                      tree-sitter-hl--query
                      root-node
                      tree-sitter-hl--query-cursor
                      nil
                      #'ts--node-text)))
      ;; TODO: Handle quitting.
      (with-silent-modifications
        (font-lock-unfontify-region beg end)
        ;; TODO: Handle uncaptured nodes.
        (seq-do #'tree-sitter-hl--highlight-capture captures))
      ;; TODO: Return the actual region being fontified.
      `(jit-lock-bounds ,beg . ,end))))

(defvar tree-sitter-hl--changed-ranges)

(defun tree-sitter-hl--extend-after-change-region (beg end _old-len)
  "Hook meant for `jit-lock-after-change-extend-region-functions'.
This relies on `tree-sitter-hl--record-changed-ranges' to have been run first."
  (when (bound-and-true-p tree-sitter-hl--changed-ranges)
    (message "tree-sitter-hl extend %s %s" beg end)
    (seq-doseq (range tree-sitter-hl--changed-ranges)
      (pcase-let ((`[,beg-byte ,end-byte] range))
        (setq beg (min beg (byte-to-position beg-byte)))
        (setq end (min end (byte-to-position end-byte)))))
    (message "                      %s %s" beg end)
    ;; TODO: Just set it to nil?
    (makunbound 'tree-sitter-hl--changed-ranges)
    `(,beg . ,end)))

(defun tree-sitter-hl--record-changed-ranges (old-tree)
  (message "tree-sitter-hl record")
  ;; TODO: What happens if more changes are made before these changes are
  ;; handled?
  (setq tree-sitter-hl--changed-ranges
        (ts-changed-ranges old-tree tree-sitter-tree)))

(defun tree-sitter-hl--invalidate (&optional old-tree)
  (if old-tree
      ;; Incremental parse.
      (seq-doseq (range (ts-changed-ranges old-tree tree-sitter-tree))
        ;; TODO: How about invalidating a single large range?
        (pcase-let* ((`[,beg-byte ,end-byte] range)
                     (beg (byte-to-position beg-byte))
                     (end (byte-to-position end-byte)))
          ;; (message "tree-sitter-hl invalidate [%s %s] %s" beg end (- end beg))
          ;; TODO: How about calling `jit-lock-refontify' directly?
          (font-lock-flush beg end)))
    ;; First parse.
    (font-lock-flush)))

;;; TODO: We want to work even without `font-lock-mode', right?
(defun tree-sitter-hl--setup ()
  "Set up `tree-sitter-hl' in the current buffer.
This assumes both `tree-sitter-mode' and `font-lock-mode' were already enabled."
  (unless tree-sitter-hl--query
    ;; XXX
    (let ((lang-symbol (alist-get major-mode tree-sitter-major-mode-language-alist)))
      (setq tree-sitter-hl--query (tree-sitter-hl--query-for lang-symbol))))
  (unless tree-sitter-hl--query-cursor
    (setq tree-sitter-hl--query-cursor (ts-make-query-cursor))
    ;; Invalidate the buffer only if we were actually disabled previously.
    (tree-sitter-hl--invalidate))

  ;; TODO: Override `font-lock-extend-after-change-region-function', or hook
  ;; into `jit-lock-after-change-extend-region-functions' directly. For that to
  ;; work, we need to make sure `tree-sitter--after-change' runs before
  ;; `jit-lock-after-change'.
  (add-hook 'tree-sitter-after-change-functions
            #'tree-sitter-hl--invalidate
            nil :local)

  ;; (add-hook 'tree-sitter-after-change-functions
  ;;           #'tree-sitter-hl--record-changed-ranges
  ;;           nil :local)
  ;; (add-function :override (local 'font-lock-extend-after-change-region-function)
  ;;               #'tree-sitter-hl--extend-after-change-region)

  ;; XXX
  (add-function :override (local 'font-lock-fontify-region-function)
                #'tree-sitter-hl--highlight-region))

(defun tree-sitter-hl--teardown ()
  "Tear down `tree-sitter-hl' in the current buffer."
  (remove-function (local 'font-lock-fontify-region-function)
                   #'tree-sitter-hl--highlight-region)

  ;; (remove-function (local 'font-lock-extend-after-change-region-function)
  ;;                  #'tree-sitter-hl--extend-after-change-region)
  ;; (remove-hook 'tree-sitter-after-change-functions
  ;;              #'tree-sitter-hl--record-changed-ranges)

  (remove-hook 'tree-sitter-after-change-functions
               #'tree-sitter-hl--invalidate
               :local)

  (setq tree-sitter-hl--query nil)
  (when tree-sitter-hl--query-cursor
    (setq tree-sitter-hl--query-cursor nil)
    ;; Invalidate the buffer only if we were actually enabled previously.
    (font-lock-flush)))

;;;###autoload
(define-minor-mode tree-sitter-hl-mode
  "Toggle syntax highlighting based on Tree-sitter's syntax tree.
Enabling this automatically enables `tree-sitter-mode' in the buffer.

To enable this automatically whenever `tree-sitter-mode' is enabled:

 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)"
  :init-value nil
  :group 'tree-sitter
  (if tree-sitter-hl-mode
      (progn
        (tree-sitter--error-protect
            (progn
              (unless tree-sitter-mode
                (tree-sitter-mode))
              (tree-sitter-hl--setup))
          (setq tree-sitter-hl-mode nil)
          (tree-sitter-hl--teardown))
        ;; Disable `tree-sitter-hl-mode' when `tree-sitter-mode' is disable.
        (add-hook 'tree-sitter--before-off-hook
                  ;; Quoting is important because we don't want a
                  ;; local-capturing closure.
                  '(lambda () (tree-sitter-hl-mode -1))
                  nil :local))
    (tree-sitter-hl--teardown)))

;;; XXX
(defun tree-sitter-hl--query-for (lang-symbol)
  (let* ((query-path (concat
                      (file-name-directory (locate-library "tree-sitter-langs"))
                      (format "/repos/tree-sitter-%s/queries/highlights.scm"
                              lang-symbol)))
         (query-string (with-temp-buffer
                         (insert-file-contents query-path)
                         (buffer-string))))
    (ts-make-query (tree-sitter-require lang-symbol) query-string)))

(provide 'tree-sitter-hl)
;;; tree-sitter-hl.el ends here
