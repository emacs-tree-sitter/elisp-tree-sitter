;;; tsc.el --- Core Tree-sitter APIs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2021  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;;         Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; Keywords: languages tools parsers dynamic-modules tree-sitter
;; Homepage: https://github.com/emacs-tree-sitter/elisp-tree-sitter
;; Version: 0.15.1
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This is the core APIs of the Emacs binding for Tree-sitter, an incremental
;; parsing system.

;;; Code:

(require 'tsc-obsolete)

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

;; Load the dynamic module at compile time as well, to satisfy the byte compiler.
(eval-and-compile
  (defconst tsc--dyn-version "0.15.1"
    "Required version of the dynamic module `tsc-dyn'.")
  (require 'tsc-dyn-get)
  (tsc-dyn-get-ensure tsc--dyn-version))

(require 'tsc-dyn)

(eval-when-compile
  (require 'pcase)
  (require 'subr-x)
  (require 'cl-lib))

(defmacro tsc--without-restriction (&rest body)
  "Execute BODY with narrowing disabled."
  (declare (indent 0))
  `(save-restriction
     (widen)
     ,@body))

(defmacro tsc--save-context (&rest body)
  "Execute BODY wrapped in a `save-excursion', with narrowing disabled."
  (declare (indent 0))
  `(save-excursion
     (tsc--without-restriction
       ,@body)))


;;; Type conversion.

(defun tsc-point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See
`tsc-parse-chunks' for a more detailed explanation."
  (tsc--save-context
    (tsc--point-from-position position)))

(defun tsc--point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.
Prefer `tsc-point-from-position', unless there's a real performance bottleneck.

This function must be called within a `tsc--save-context' block."
  (goto-char position)
  (let ((line-number (line-number-at-pos position))
        ;; TODO: Add tests that fail if `current-column' is used instead.
        (byte-column (- (position-bytes position)
                        (position-bytes (line-beginning-position)))))
    (cons line-number byte-column)))

(defun tsc-point-to-position (point)
  "Convert tree-sitter POINT to buffer position.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See
`tsc-parse-chunks' for a more detailed explanation."
  (tsc--save-context
    (let ((line-number (car point))
          (byte-column (cdr point)))
      (goto-char 1)
      (forward-line (- line-number 1))
      (byte-to-position (+ byte-column (position-bytes (line-beginning-position)))))))


;;; Extracting buffer's text.

(defun tsc--buffer-input (bytepos _line-number _byte-column)
  "Return a portion of the current buffer's text, starting from BYTEPOS.
BYTEPOS is automatically clamped to the range valid for the current buffer.

This function must be called with narrowing disabled, e.g. within a
`tsc--without-restriction' block."
  (let* ((max-pos (point-max))
         (beg-byte (max 1 bytepos))
         ;; ;; TODO: Don't hard-code read length.
         (end-byte (+ 1024 beg-byte))
         ;; nil means > max-pos, since we already made sure they are non-negative.
         (beg-pos (or (byte-to-position beg-byte) max-pos))
         (end-pos (or (byte-to-position end-byte) max-pos)))
    (buffer-substring-no-properties beg-pos end-pos)))

(defun tsc--buffer-substring-no-properties (beg-byte end-byte)
  "Return the current buffer's text from BEG-BYTE to END-BYTE.
This function must be called with narrowing disabled, e.g. within a
`tsc--without-restriction' block."
  (buffer-substring-no-properties
   (byte-to-position beg-byte)
   (byte-to-position end-byte)))

(defun tsc--node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree.
Prefer `tsc-node-text', unless there's a real bottleneck.

This function must be called with narrowing disabled, e.g. within a
`tsc--without-restriction' block."
  (pcase-let ((`(,beg . ,end) (tsc-node-position-range node)))
    (buffer-substring-no-properties beg end)))

(defun tsc-node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree."
  (tsc--without-restriction
    (tsc--node-text node)))


;;; Convenient versions of some functions.

(defun tsc-get-descendant-for-position-range (node beg end)
  "Return the smallest node within NODE that spans the range (BEG . END).
This function must be called in NODE's source buffer."
  (tsc-get-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun tsc-get-named-descendant-for-position-range (node beg end)
  "Return the smallest named node within NODE that spans the range (BEG . END).
This function must be called in NODE's source buffer."
  (tsc-get-named-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun tsc-get-child-by-field (node field)
  "Return NODE's child associated with FIELD, which should be a keyword."
  (unless (keywordp field)
    (signal 'wrong-type-argument (list 'keywordp field)))
  (tsc--get-child-by-field-name node (substring (symbol-name field) 1)))

(defun tsc-node-start-position (node)
  "Return NODE's start position.
This function must be called in NODE's source buffer."
  (byte-to-position (tsc-node-start-byte node)))

(defun tsc-node-end-position (node)
  "Return NODE's end position.
This function must be called in NODE's source buffer."
  (byte-to-position (tsc-node-end-byte node)))

(defun tsc-node-position-range (node)
  "Return NODE's (START-POSITION . END-POSITION).
This function must be called in NODE's source buffer."
  (let ((range (tsc-node-byte-range node)))
    (cl-callf byte-to-position (car range))
    (cl-callf byte-to-position (cdr range))
    range))

(defun tsc-goto-first-child-for-position (cursor position)
  "Move CURSOR to the first child that extends beyond the given POSITION.
Return the index of the child node if one was found, nil otherwise."
  (tsc-goto-first-child-for-byte cursor (position-bytes position)))

(defun tsc-lang-field-id (language field)
  "Return the numeric id of FIELD in LANGUAGE. FIELD should be a keyword."
  (unless (keywordp field)
    (signal 'wrong-type-argument (list 'keywordp field)))
  (tsc--lang-field-id-for-name language (substring (symbol-name field) 1)))

(defun tsc-lang-node-type-id (language node-type)
  "Return the numeric id of NODE-TYPE in LANGUAGE.
NODE-TYPE should be a symbol (named nodes) or a string (anonymous nodes)."
  (cond
   ((symbolp node-type)
    (tsc--lang-type-id-for-name language (symbol-name node-type) :named))
   (t
    (tsc--lang-type-id-for-name language node-type nil))))


;;; Querying.

(defun tsc--stringify-patterns (patterns)
  "Convert PATTERNS into a query string that can be passed to `tsc--make-query'."
  (cond
   ((stringp patterns) patterns)
   ((sequencep patterns)
    ;; XXX: This is hacky.
    (thread-last (mapconcat (lambda (p) (format "%S" p)) patterns "\n")
      (replace-regexp-in-string (regexp-quote "\\?") "?")
      (replace-regexp-in-string (regexp-quote "\\.") ".")))
   (t (error "Invalid patterns"))))

(defun tsc-make-query (language patterns &optional tag-assigner)
  "Create a new query for LANGUAGE from a sequence of S-expression PATTERNS.
The query is associated with LANGUAGE, and can only be run on syntax nodes
parsed with LANGUAGE.

When the query is executed, each captured node is tagged with a symbol, whose
name is the corresponding capture name defined in PATTERNS. For example, nodes
that are captured as \"@function.builtin\" will be tagged with the symbol
`function.builtin'. This behavior can be customized by the optional function
TAG-ASSIGNER, which should return a tag value when given a capture name (without
the prefix \"@\"). If it returns nil, the associated capture name is disabled.

See also: `tsc-query-captures' and `tsc-query-matches'."
  (tsc--make-query language (tsc--stringify-patterns patterns)
                  (or tag-assigner #'intern)))

(defun tsc-query-matches (query node text-function &optional cursor)
  "Execute QUERY on NODE and return a sequence of matches.
Matches are sorted in the order they were found.

Each match has the form (PATTERN-INDEX . MATCH-CAPTURES), where PATTERN-INDEX is
the 0-based position of the matched pattern within QUERY, and MATCH-CAPTURES is
a sequence of captures associated with the match, similar to that returned by
`tsc-query-captures'.

TEXT-FUNCTION is called to get nodes' texts (for text-based predicates). It
should take 2 parameters: (BEG-BYTE END-BYTE), and return the corresponding
chunk of text in the source code.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise, a newly created query-cursor is used."
  (tsc--query-cursor-matches
   (or cursor (tsc-make-query-cursor)) query node text-function))

(defun tsc-query-captures (query node text-function &optional cursor)
  "Execute QUERY on NODE and return a sequence of captures.
Captures are sorted in the order they appear.

Each capture has the form (CAPTURE-TAG . CAPTURED-NODE), where CAPTURE-TAG is a
symbol, whose name is the corresponding capture name defined in QUERY (without
the prefix \"@\"). If QUERY was created with a custom tag assigner, CAPTURE-TAG
is the value returned by that function instead. See also: `tsc-make-query'.

TEXT-FUNCTION is called to get nodes' texts (for text-based predicates). It
should take 2 parameters: (BEG-BYTE END-BYTE), and return the corresponding
chunk of text in the source code.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise, a newly created query-cursor is used."
  (tsc--query-cursor-captures
   (or cursor (tsc-make-query-cursor)) query node text-function))


;;; Utilities.

(defun tsc-pp-to-string (tree)
  "Return the pretty-printed string of TREE's sexp."
  (pp-to-string (read (tsc-tree-to-sexp tree))))

(defun tsc--node-steps (node)
  "Return the sequence of steps from the root node to NODE.

Each step has the form (CHILD-NODE . NTH), where CHILD-NODE is the node to
descend into, and NTH is its 0-based ordinal position within the parent node.

If NODE is the root node, the sequence is empty."
  (let ((steps)
        (parent)
        (this node))
    (while (setq parent (tsc-get-parent this))
      (push (catch :tsc-step
              (let ((i 0))
                (tsc-mapc-children (lambda (child)
                                    (if (tsc-node-eq child this)
                                        (throw :tsc-step (cons this i))
                                      (setq i (1+ i))))
                                  parent))
              (throw :tsc-is-not-parents-child (cons this parent)))
            steps)
      (setq this parent))
    steps))

(define-error 'tsc--invalid-node-step "Cannot follow node step")

(defun tsc--node-from-steps (tree steps)
  "Follow STEPS from TREE's root node; return the final node.
STEPS should be a sequence of steps, as described by `tsc--node-steps'.

If a step cannot be followed, signal a `tsc--invalid-node-step' error."
  (let ((this (tsc-root-node tree)))
    (pcase-dolist (`(,old-node . ,i) steps)
      (let ((new-node (tsc-get-nth-child this i)))
        (unless new-node
          (signal 'tsc--invalid-node-step (list this old-node i new-node)))
        (let ((new-type (tsc-node-type new-node))
              (old-type (tsc-node-type old-node)))
          (unless (equal old-type new-type)
            (signal 'tsc--invalid-node-step (list this old-node i new-node))))
        (setq this new-node)))
    this))

(provide 'tsc)
;;; tsc.el ends here
