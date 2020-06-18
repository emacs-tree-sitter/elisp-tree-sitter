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

;;; ----------------------------------------------------------------------------
;;; Faces for commonly used highlight names.

(defgroup tree-sitter-hl nil
  "Syntax highlighting using tree-sitter."
  :group 'tree-sitter)

(defgroup tree-sitter-hl-faces nil
  "All the faces of tree-sitter."
  :group 'tree-sitter-hl)

(defface tree-sitter-hl-face:attribute '((default :inherit font-lock-preprocessor-face))
  "Face used for attribute"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:comment '((default :inherit font-lock-comment-face))
  "Face used for comment"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:constant '((default :inherit font-lock-constant-face))
  "Face used for constant"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:constant.builtin '((default :inherit font-lock-builtin-face))
  "Face used for constant.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:escape '((default :inherit font-lock-keyword-face))
  "Face used for escape"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:function '((default :inherit font-lock-function-name-face))
  "Face used for function"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:function.call '((default :inherit (link font-lock-function-name-face) :underline nil))
  "Face used for function.call"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:function.builtin '((default :inherit font-lock-builtin-face))
  "Face used for function.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:function.macro '((default :inherit font-lock-preprocessor-face))
  "Face used for function.macro"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:function.special '((default :inherit font-lock-preprocessor-face))
  "Face used for function.special"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:method '((default :inherit tree-sitter-hl-face:function))
  "Face used for method"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:method.call '((default :inherit tree-sitter-hl-face:function.call))
  "Face used for method.call"
  :group 'tree-sitter-hl-faces)

(define-obsolete-face-alias 'tree-sitter-hl-face:function.method
  'tree-sitter-hl-face:method "0.9.0")

(define-obsolete-face-alias 'tree-sitter-hl-face:function.method.call
  'tree-sitter-hl-face:method.call "0.9.0")

(defface tree-sitter-hl-face:keyword '((default :inherit font-lock-keyword-face))
  "Face used for keyword"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:label '((default :inherit font-lock-preprocessor-face))
  "Face used for label"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:operator '((default :inherit font-lock-keyword-face))
  "Face used for operator"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:property '((default :inherit font-lock-constant-face :slant italic))
  "Face used for property"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:property.definition '((default :inherit tree-sitter-hl-face:variable.parameter))
  "Face used for property.definition"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation '(())
  "Face used for punctuation"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.bracket '((default :inherit tree-sitter-hl-face:punctuation))
  "Face used for punctuation.bracket"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:punctuation.delimiter '((default :inherit tree-sitter-hl-face:punctuation))
  "Face used for punctuation.delimiter"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:string '((default :inherit font-lock-string-face))
  "Face used for string"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:doc '((default :inherit font-lock-doc-face))
  "Face used for doc"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:embedded '((default :inherit default))
  "Face used for embedded"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:type '((default :inherit font-lock-type-face))
  "Face used for type"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:type.parameter '((default :inherit font-lock-variable-name-face))
  "Face used for type.parameter"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:type.argument '((default :inherit tree-sitter-hl-face:type))
  "Face used for type.argument"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:type.builtin '((default :inherit font-lock-builtin-face))
  "Face used for type.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:type.super '((default :inherit tree-sitter-hl-face:type))
  "Face used for type.super"
  :group 'tree-sitter-hl-faces)

;; TODO: Decide whether we really need this.
(defface tree-sitter-hl-face:constructor '((default :inherit tree-sitter-hl-face:type))
  "Face used for constructor"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable '((default :inherit font-lock-variable-name-face))
  "Face used for variable"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.use '((default :inherit default))
  "Face used for variable.use"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.builtin '((default :inherit font-lock-builtin-face))
  "Face used for variable.builtin"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:variable.parameter '((default :inherit tree-sitter-hl-face:variable))
  "Face used for variable.parameter"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:tag '((default :inherit font-lock-builtin-face))
  "Face used for tag"
  :group 'tree-sitter-hl-faces)

(defface tree-sitter-hl-face:number '((default :inherit font-lock-constant-face))
  "Face used for number"
  :group 'tree-sitter-hl-faces)

;;; ----------------------------------------------------------------------------
;;; Interfaces for modes and end users.

(defcustom tree-sitter-hl-use-font-lock-keywords :except-font-lock-defaults
  "Whether to keep using the highlighting provided by `font-lock-keywords'.
If `:except-font-lock-defaults', then keywords specified by `font-lock-defaults'
are ignored, while keywords added through `font-lock-add-keywords' are used. The
former is typically set by major modes, while the latter is usually set by minor
modes and end users."
  :group 'tree-sitter-hl
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Except `font-lock-defaults'" :except-font-lock-defaults)))

(defcustom tree-sitter-hl-face-mapping-function
  #'tree-sitter-hl-face-from-common-scope
  "Function used to map capture names in query patterns to highlighting faces.
This can also be used to selectively disable certain capture names. For example,
the following code disables keyword highlighting:

 (add-function :before-while 'tree-sitter-hl-face-mapping-function
               (lambda (capture-name)
                 (not (string= capture-name \"keyword\"))))"
  :group 'tree-sitter-hl
  :type 'function)

(defvar-local tree-sitter-hl-default-patterns nil
  "Default syntax highlighting patterns.
This should be set by major modes that want to integrate with `tree-sitter-hl'.
It plays a similar role to `font-lock-defaults'.")

(defvar tree-sitter-hl--patterns-alist nil
  "Additional language-specific syntax highlighting patterns.
It plays a similar role to `font-lock-keywords-alist', except that its keys are
language symbols, not major mode symbols.")

(defvar-local tree-sitter-hl--extra-patterns-list nil
  "Additional buffer-local syntax highlighting patterns.")

(defvar-local tree-sitter-hl--query nil
  "Tree query used for syntax highlighting, compiled from patterns.")

(defun tree-sitter-hl--ensure-query ()
  "Return the tree query to be used for syntax highlighting in this buffer."
  (unless tree-sitter-hl--query
    (setq tree-sitter-hl--query
          (ts-make-query
           tree-sitter-language
           (mapconcat #'ts--stringify-patterns
                      (append tree-sitter-hl--extra-patterns-list
                              (list tree-sitter-hl-default-patterns))
                      "\n")
           tree-sitter-hl-face-mapping-function)))
  tree-sitter-hl--query)

(defun tree-sitter-hl-face-from-common-scope (capture-name)
  "Return the default face used to highlight CAPTURE-NAME."
  ;; TODO: If a scope does not have a corresponding face, check its ancestors.
  (intern (format "tree-sitter-hl-face:%s" capture-name)))

;;; TODO: Support adding/removing language-specific patterns.
(defun tree-sitter-hl-add-patterns (patterns)
  "Add buffer-local syntax highlighting PATTERNS.
These will take precedence over `tree-sitter-hl-default-patterns', as well as
previously added patterns."
  ;; Do nothing if the patterns are already on top.
  (unless (equal patterns (cl-first tree-sitter-hl--extra-patterns-list))
    (let ((old-list tree-sitter-hl--extra-patterns-list)
          (old-query tree-sitter-hl--query))
      ;; Update the patterns list and request the query to be rebuilt...
      (setq tree-sitter-hl--extra-patterns-list
            (append (list patterns) (remove patterns old-list)))
      (setq tree-sitter-hl--query nil)
      ;; ... and build it if possible. During a major mode's hook, we may not
      ;; even know the language, in which case we let `tree-sitter-hl--setup'
      ;; build the query later on.
      (when tree-sitter-language
        (tree-sitter--error-protect (tree-sitter-hl--ensure-query)
          ;; When the newly added patterns are invalid, restore the old state.
          (setq tree-sitter-hl--query old-query
                tree-sitter-hl--extra-patterns-list old-list))
        ;; Everything is in place. Request a re-render.
        (when (bound-and-true-p tree-sitter-hl-mode)
          (tree-sitter-hl--invalidate))))))

;;; ----------------------------------------------------------------------------
;;; Internal workings.

(defvar-local tree-sitter-hl--query-cursor nil)

(defconst tree-sitter-hl--extend-region-limit 2048
  "The max size of the extended region, in characters.")

(defconst tree-sitter-hl--extend-region-levels 4
  "The max number of levels to walk up the syntax tree to extend the region.")

(defun tree-sitter-hl--extend-region (beg end)
  "Return a \"safe\" region that encloses (BEG . END), to run the query on.
Because a match is returned only when all nodes in the pattern intersect the
query cursor's range, relying on `ts-changed-ranges' alone is insufficient.

Another pathological case is `jit-lock--run-funtions' being called on a very
small region. An example is when `evil-adjust-cursor' triggers a
`vertical-motion' (outside of a redisplay).

See https://github.com/tree-sitter/tree-sitter/issues/598."
  (pcase-let* ((region `(,beg . ,end))
               (root-node (ts-root-node tree-sitter-tree))
               (node (ts-get-descendant-for-position-range root-node beg end))
               (`(,beg . ,end) (ts-node-position-range node))
               (level 0))
    ;; Repeatedly extend the region, within the limit. TODO: What if the region
    ;; of the minimal enclosing node is already too large?
    (while (and node
                (< (- end beg) tree-sitter-hl--extend-region-limit))
      (setcar region beg)
      (setcdr region end)
      ;; Walk up to the parent node.
      (when (setq node (when (<= (cl-incf level)
                                 tree-sitter-hl--extend-region-levels)
                         (ts-get-parent node)))
        (let ((range (ts-node-position-range node)))
          (setf `(,beg . ,end) range))))
    ;; TODO: Extend to whole lines?
    region))

(defun tree-sitter-hl--append-text-property (start end prop value &optional object)
  "Append VALUE to PROP of the text from START to END.
This is similar to `font-lock-append-text-property', but deduplicates values. It
also expects VALUE to be a single value, not a list."
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

(defun tree-sitter-hl--prepend-text-property (start end prop value &optional object)
  "Prepend VALUE to PROP of the text from START to END.
This is similar to `font-lock-prepend-text-property', but deduplicates values. It
also expects VALUE to be a single value, not a list."
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
                           (cons value prev)
                           object))
      (setq start next))))

(defun tree-sitter-hl--highlight-capture (capture)
  "Highlight the given CAPTURE."
  (pcase-let ((`(,face . (,beg-byte . ,end-byte)) capture))
    (when (facep face)
      (tree-sitter-hl--append-text-property
       (byte-to-position beg-byte)
       (byte-to-position end-byte) 'face face))))

;;; TODO: Handle embedded DSLs (injections).
(defun tree-sitter-hl--highlight-region (beg end &optional loudly)
  "Highlight the region (BEG . END).
This is intended to be used as a buffer-local override of
`font-lock-fontify-region-function'."
  (ts--save-context
    ;; Extend the region to be highlighted, based on some heuristics, so that
    ;; querying works in certain pathological cases. This is analogous to the
    ;; extension done by `font-lock-default-fontify-region'. TODO: Consider
    ;; distinguishing region to query from region to fontify.
    (let ((region (tree-sitter-hl--extend-region beg end)))
      (setf `(,beg . ,end) region))
    (ts--query-cursor-set-byte-range tree-sitter-hl--query-cursor
                                     (position-bytes beg)
                                     (position-bytes end))
    (let* ((root-node (ts-root-node tree-sitter-tree))
           (captures  (ts--query-cursor-captures-1
                       tree-sitter-hl--query-cursor
                       tree-sitter-hl--query
                       root-node
                       #'ts--buffer-substring-no-properties)))
      ;; TODO: Handle quitting.
      (let ((inhibit-point-motion-hooks t))
        (with-silent-modifications
          (font-lock-unfontify-region beg end)
          ;; TODO: Consider giving certain combinations of highlight names their
          ;; own faces. For example, it might be desirable for fontification of
          ;; a node that matches both "constructor" and "variable" to be
          ;; different from the union of "constructor fontification" and
          ;; "variable fontification".
          (mapc #'tree-sitter-hl--highlight-capture captures)
          ;; This should primarily be for keywords added through
          ;; `font-lock-add-keywords' (minor modes and end users).
          (when (and tree-sitter-hl-use-font-lock-keywords
                     font-lock-set-defaults)
            (font-lock-fontify-keywords-region beg end loudly))))
      ;; TODO: Return the actual region being fontified.
      `(jit-lock-bounds ,beg . ,end))))

(defun tree-sitter-hl--invalidate (&optional old-tree)
  "Mark regions of text to be rehighlighted after a text change.
Installed on `tree-sitter-after-change-functions'.

OLD-TREE is the tree before the edit."
  (if old-tree
      ;; Incremental parse.
      (seq-doseq (range (ts-changed-ranges old-tree tree-sitter-tree))
        ;; TODO: How about invalidating a single large range?
        (pcase-let* ((`[,beg-byte ,end-byte] range)
                     (beg (byte-to-position beg-byte))
                     (end (byte-to-position end-byte)))
          ;; TODO: How about calling `jit-lock-refontify' directly?
          (font-lock-flush beg end)))
    ;; First parse.
    (font-lock-flush)))

;;; ----------------------------------------------------------------------------
;;; Setup and teardown.

(defvar-local tree-sitter-hl--font-lock-keywords nil)

(defun tree-sitter-hl--minimize-font-lock-keywords ()
  "Remove keywords set by `font-lock-defaults' from `font-lock-keywords'."
  (when font-lock-set-defaults
    (unless tree-sitter-hl--font-lock-keywords
      (setq tree-sitter-hl--font-lock-keywords font-lock-keywords)
      (when (eq tree-sitter-hl-use-font-lock-keywords :except-font-lock-defaults)
        ;; XXX: Check whether this covers all the edge cases of the interaction
        ;; between `font-lock-eval-keywords' and `font-lock-remove-keywords'.
        (let* ((keywords-spec (car font-lock-defaults))
               ;; The spec can be a list, corresponding to multiple levels of
               ;; fontification. We want to disable all of them.
               (keywords-list (if (and (listp keywords-spec)
                                       (symbolp (car keywords-spec)))
                                  keywords-spec
                                (list keywords-spec))))
          (dolist (keywords keywords-list)
            (font-lock-remove-keywords
             nil (font-lock-eval-keywords keywords))))))))

(defun tree-sitter-hl--restore-font-lock-keywords ()
  "Undo the hack done by `tree-sitter-hl--minimize-font-lock-keywords'."
  (when font-lock-set-defaults
    (when tree-sitter-hl--font-lock-keywords
      (setq font-lock-keywords tree-sitter-hl--font-lock-keywords
            tree-sitter-hl--font-lock-keywords nil))))

;;; TODO: We want to work even without `font-lock-mode', right?
(defun tree-sitter-hl--setup ()
  "Set up `tree-sitter-hl' in the current buffer.
This assumes both `tree-sitter-mode' and `font-lock-mode' were already enabled."
  ;; TODO: If there's an error, disable `tree-sitter-hl--extra-patterns-list'
  ;; and retry.
  (tree-sitter-hl--ensure-query)
  (unless tree-sitter-hl--query-cursor
    (setq tree-sitter-hl--query-cursor (ts-make-query-cursor))
    ;; Invalidate the buffer, only if we were actually disabled previously.
    (tree-sitter-hl--invalidate))
  ;; TODO: Override `font-lock-extend-after-change-region-function', or hook
  ;; into `jit-lock-after-change-extend-region-functions' directly. For that to
  ;; work, we need to make sure `tree-sitter--after-change' runs before
  ;; `jit-lock-after-change'.
  (add-hook 'tree-sitter-after-change-functions
            #'tree-sitter-hl--invalidate
            nil :local)
  ;; XXX
  (add-function :override (local 'font-lock-fontify-region-function)
                #'tree-sitter-hl--highlight-region)
  (tree-sitter-hl--minimize-font-lock-keywords)
  ;; When `font-lock-defaults' is not set up, `font-lock-mode' only does a
  ;; partial initialization. In that case, we initialize it directly. This
  ;; allows turning on tree-based syntax highlighting by temporarily binding
  ;; `major-mode', even though such a major mode may not be installed, or does
  ;; not exist. For example:
  ;;
  ;;     (let ((major-mode 'go-mode)) (tree-sitter-hl-mode))
  ;;
  ;; TODO: Figure out how to properly integrate with `jit-lock-mode' directly,
  ;; e.g. so that fontification is updated in-time, instead of eventually in
  ;; some cases.
  (unless font-lock-set-defaults
    (font-lock-turn-on-thing-lock)))

(defun tree-sitter-hl--teardown ()
  "Tear down `tree-sitter-hl' in the current buffer."
  (remove-function (local 'font-lock-fontify-region-function)
                   #'tree-sitter-hl--highlight-region)
  (remove-hook 'tree-sitter-after-change-functions
               #'tree-sitter-hl--invalidate
               :local)
  (setq tree-sitter-hl--query nil)
  (when tree-sitter-hl--query-cursor
    (setq tree-sitter-hl--query-cursor nil)
    ;; Invalidate the buffer, only if we were actually enabled previously.
    (font-lock-flush))
  (tree-sitter-hl--restore-font-lock-keywords)
  ;; If we did a hackish initialization of `font-lock-mode', de-initialize it.
  (unless font-lock-set-defaults
    (font-lock-unfontify-buffer)
    (font-lock-turn-off-thing-lock)))

;;;###autoload
(define-minor-mode tree-sitter-hl-mode
  "Toggle syntax highlighting based on Tree-sitter's syntax tree.
Enabling this automatically enables `tree-sitter-mode' in the buffer.

To enable this automatically whenever `tree-sitter-mode' is enabled:

 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)"
  :init-value nil
  :group 'tree-sitter
  (tree-sitter--handle-dependent tree-sitter-hl-mode
    #'tree-sitter-hl--setup
    #'tree-sitter-hl--teardown))

(provide 'tree-sitter-hl)
;;; tree-sitter-hl.el ends here
