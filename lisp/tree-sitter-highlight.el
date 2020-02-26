;;; tree-sitter-highlight.el --- Highlighting of buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Timo von Hartz
;;
;; Author: Timo von Hartz <c0untlizzi@gmail.com>

;;; Commentary:

;; Very early implementation of highlighting.
;;; Code:

(require 'tree-sitter)

(defgroup tree-sitter-highlight nil
  "Syntax highlighting using tree-sitter."
  :group 'tree-sitter)

(defgroup tree-sitter-highlight-faces nil
  "All the faces of tree-sitter."
  :group 'tree-sitter-highlight)

(defface tree-sitter-attribute-face '((default :inherit font-lock-preprocessor-face))
  "Face used for attribute"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-comment-face '((default :inherit font-lock-comment-face))
  "Face used for comment"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-constant-face '((default :inherit font-lock-constant-face))
  "Face used for constant"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-constant-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for constant.builtin"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-constructor-face '((default :inherit font-lock-variable-name-face))
  "Face used for constructor"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-escape-face '(())
  "Face used for escape"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-function-face '((default :inherit font-lock-function-name-face))
  "Face used for function"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-function-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for function.builtin"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-function-macro-face '((default :inherit font-lock-function-name-face))
  "Face used for function.macro"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-function-method-face '((default :inherit font-lock-function-name-face))
  "Face used for function.method"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-identifier-face '((default :inherit font-lock-function-name-face))
  "Face used for identifier"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-keyword-face '((default :inherit font-lock-keyword-face))
  "Face used for keyword"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-label-face '((default :inherit font-lock-preprocessor-face))
  "Face used for label"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-operator-face '(())
  "Face used for operator"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-property-face '((default :inherit font-lock-variable-name-face))
  "Face used for property"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-punctuation-face '(())
  "Face used for punctuation"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-punctuation-bracket-face '(())
  "Face used for punctuation.bracket"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-punctuation-delimiter-face '(())
  "Face used for punctuation.delimiter"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-string-face '((default :inherit font-lock-string-face))
  "Face used for string"
  :group 'tree-sitter-highlight-faces)

(defface tree-sitter-type-face '((default :inherit font-lock-type-face))
  "Faced used for type"
  :group 'tree-sitter-highlight-face)

(defface tree-sitter-type-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for type.builtin"
  :group 'tree-sitter-highlight-face)

(defface tree-sitter-variable-face '((default :inherit font-lock-variable-name-face))
  "Face used for variable"
  :group 'tree-sitter-highlight-face)

(defface tree-sitter-variable-builtin-face '((default :inherit font-lock-builtin-face))
  "Face used for variable.builtin"
  :group 'tree-sitter-highlight-face)

(defface tree-sitter-variable-parameter-face '((default :inherit font-lock-variable-name-face))
  "Faced used for variable.parameter"
  :group 'tree-sitter-highlight-face)

(defcustom tree-sitter-highlight-default-faces
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
  :group 'tree-sitter-highlight)

(defcustom tree-sitter-highlight-query-dir nil
  "Where queries for languages are stored.
Directory needs to look as follows:
`tree-sitter-highlight-query-dir'/tree-sitter-<language>/queries/highlights.scm"
  :type '(choice (const :tag "none" nil)
           (directory :tag "path"))
  :group 'tree-sitter-highlight)

(defcustom tree-sitter-highlight-setup-functions nil
  "Functions to call before initializing the highlighting system.
These functions could alter the queries or mapping of query identifiers
to faces.  Each function takes no arguments."
  :type 'hook
  :group 'tree-sitter-highlight)

(defvar-local tree-sitter-highlight--capture-names nil)
(defvar-local tree-sitter-highlight--face-hash nil
  "Hashtable from query identifier to face, built from
`tree-sitter-highlight-default-faces' and `tree-sitter-highlight-buffer-faces'.")
(defvar-local tree-sitter-highlight--injections nil)
(defvar-local tree-sitter-highlight--injections-query nil)
(defvar-local tree-sitter-highlight--jit-function nil)
(defvar-local tree-sitter-highlight--orig-scroll-functions nil)
(defvar-local tree-sitter-highlight--query nil)
(defvar-local tree-sitter-highlight--query-cursor nil)

(defvar-local tree-sitter-highlight--orig-buffer-function nil)
(defvar-local tree-sitter-highlight--orig-region-function nil)

(defun tree-sitter-highlight--read-file (language file)
  "Read FILE from the queries directory for the given LANGUAGE."
  (let ((path (concat tree-sitter-highlight-query-dir
                "/"
                (format "tree-sitter-%s"
                  (symbol-name language))
                "/queries/"
                file)))
    (when (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (buffer-string)))))

(defun tree-sitter-highlight--make-queries (language)
  "Parse a highlights.scm file and return a query."
  (let* ((highlights (when tree-sitter-highlight-query-dir
                       (if (eql language 'cpp)
                         (concat (tree-sitter-highlight--read-file language "highlights.scm")
                           (tree-sitter-highlight--read-file 'c "highlights.scm"))
                         (or (tree-sitter-highlight--read-file language "highlights.scm")
                           (error "No highlights found for %s" language)))))
          (injections (when tree-sitter-highlight-query-dir
                        (tree-sitter-highlight--read-file language "injections.scm")))
          ;; (lang (tree-sitter--get-or-load-language language))
          (lang tree-sitter-language)
          (query (ts--make-query lang highlights))
          ;; If we don't find any injections just ignore that.
          (injections-query (ts--make-query lang (or injections ""))))
    `(,query ,injections-query)))

(defun tree-sitter-highlight--apply (x)
  "Apply the face for the match X in the buffer."
  (let* ((node  (cdr x))
          ;; (index (car x))
          ;; (name  (aref tree-sitter-highlight--capture-names index))
          (name (car x))
          (face (or (gethash name tree-sitter-highlight--face-hash)
                  (gethash (car (split-string name "\\.")) tree-sitter-highlight--face-hash)))
          (start (ts-node-start-position node))
          (end   (ts-node-end-position node)))
    (unless (get-text-property start 'face)
      (add-face-text-property start end face))))

(defun tree-sitter-highlight--get-injection (language)
  (cond ((eq language major-mode) '(tree-sitter-highlight--query
                                     tree-sitter-highlight--injections-query))
    (t (or (alist-get language tree-sitter-highlight--injections)
         (let ((x (tree-sitter-highlight--make-queries language)))
           (push '(language . x) tree-sitter-highlight--injections)
           x)))))

(defun tree-sitter-highlight--get-matches (start end)
  (ts-set-point-range tree-sitter-highlight--query-cursor
    (ts--point-from-position start)
    (ts--point-from-position end))
  (ts--query-cursor-matches
    tree-sitter-highlight--query-cursor
    tree-sitter-highlight--query
    (ts-root-node tree-sitter-tree)
    nil
    ;; t
    #'ts-node-text)
  )

(defun tree-sitter-highlight--jit (old-tree)
  (when old-tree
    (let ((changes (ts-changed-ranges old-tree tree-sitter-tree))
           (wstart (window-start))
           (wend (window-end)))
      ;; TODO: Remember what we've highlighted, similar to how font-lock does it.
      ;;       Already highlighted regions shouldn't be re-highlighted.

      ;; Find changes that are within the current window
      (mapc #'(lambda (range)
                (let ((start (aref range 0))
                      (end (aref range 1)))
                  ;; TODO: Improve this
                  (tree-sitter-highlight--highlight (max wstart start) (min wend end))))
        changes))))

(defun tree-sitter-highlight--highlight (start end)
  (ts--save-context
    (with-silent-modifications
      (remove-text-properties start
        end
        '(face nil))
      (let ((matches (tree-sitter-highlight--get-matches start end)))
        (seq-do #'(lambda (match)
                    (seq-do #'tree-sitter-highlight--apply (cdr match)))
          matches)))))
;; (seq-do #'tree-sitter-highlight--apply

;; (ts-highlight (ts-root-node tree-sitter-tree)
;;               tree-sitter-highlight--query-cursor
;;               tree-sitter-highlight--query
;;               tree-sitter-highlight--injections-query
;;               #'ts-node-text
;;               (lambda (start end)
;;                 (remove-text-properties (ts-byte-to-position start)
;;                                         (ts-byte-to-position end)
;;                                         '(face nil)))
;;               (lambda (language)
;;                 (car (tree-sitter-highlight--get-injection (intern language))))
;;               (lambda (language)
;;                 (cadr (tree-sitter-highlight--get-injection (intern language))))
;;               (ts-byte-from-position start)
;;               (ts-byte-from-position end)))))

(defun tree-sitter-highlight--highlight-window (_window start)
  (tree-sitter-highlight--highlight start (window-end nil t)))

(defun tree-sitter-highlight--enable ()
  "Enable `tree-sitter-highlight' in this buffer."
  (run-hooks  'tree-sitter-highlight-setup-functions)
  (setq tree-sitter-highlight--face-hash
    (let ((table (make-hash-table :test 'equal)))
      (mapc (lambda (x)
              (pcase-let ((`(,key . ,value) x))
                (puthash key value table)))
        tree-sitter-highlight-default-faces)
      table))
  (let ((x (tree-sitter-highlight--make-queries (alist-get major-mode
                                                  tree-sitter-major-mode-language-alist))))
    (setq tree-sitter-highlight--query (car x)
      tree-sitter-highlight--injections-query (cadr x)))
  (setq tree-sitter-highlight--capture-names (ts-query-capture-names tree-sitter-highlight--query))
  (setq tree-sitter-highlight--query-cursor (ts-make-query-cursor))
  (make-variable-buffer-local 'window-scroll-functions)
  (setq tree-sitter-highlight--orig-scroll-functions window-scroll-functions)
  (setq window-scroll-functions (cons #'tree-sitter-highlight--highlight-window window-scroll-functions))
  (tree-sitter-highlight--highlight-window nil (window-start))
  (add-hook 'tree-sitter-after-change-functions #'tree-sitter-highlight--jit nil t)
  )

(defun tree-sitter-highlight--disable ()
  "Disable `tree-sitter-highlight' in this buffer."
  (with-silent-modifications
    (remove-text-properties (point-min)
      (point-max)
      '(face nil)))
  (setq window-scroll-functions tree-sitter-highlight--orig-scroll-functions)
  (remove-hook 'tree-sitter-after-change-functions #'tree-sitter-highlight--jit t))

(define-minor-mode tree-sitter-highlight-mode
  "Syntax highlighting with tree sitter."
  :init-value nil
  :lighter "tree-sitter-highlight"
  (tree-sitter-mode)
  (if tree-sitter-highlight-mode
    (let ((err t))
      (unwind-protect
        (prog1 (tree-sitter-highlight--enable)
          (setq err nil))
        (when err
          (setq tree-sitter-highlight-mode nil))))
    (tree-sitter-highlight--disable)))

(provide 'tree-sitter-highlight)
;;; tree-sitter-highlight.el ends here
