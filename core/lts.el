;;; lts.el --- Core tree-sitter APIs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;;         Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; Keywords: languages tools parsers dynamic-modules tree-sitter
;; Homepage: https://github.com/ubolonton/emacs-tree-sitter
;; Version: 0.11.1
;; Package-Requires: ((emacs "25.1"))
;; License: MIT

;;; Commentary:

;; This is the core Emacs Lisp bindings for Tree-sitter (`lts'), an incremental
;; parsing system.

;;; Code:

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

;; Load the dynamic module at compile time as well, to satisfy the byte compiler.
(eval-and-compile
  (defconst lts--dyn-version "0.11.0"
    "Required version of the dynamic module `lts-dyn'.")
  (require 'lts-dyn-get)
  (lts-dyn-get-ensure lts--dyn-version))

(require 'lts-dyn)

(eval-when-compile
  (require 'pcase)
  (require 'subr-x)
  (require 'cl-lib))

(defmacro lts--without-restriction (&rest body)
  "Execute BODY with narrowing disabled."
  (declare (indent 0))
  `(save-restriction
     (widen)
     ,@body))

(defmacro lts--save-context (&rest body)
  "Execute BODY wrapped in a `save-excursion', with narrowing disabled."
  (declare (indent 0))
  `(save-excursion
     (lts--without-restriction
       ,@body)))


;;; Type conversion.

(defun lts-point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See
`lts-parse-chunks' for a more detailed explanation."
  (lts--save-context
    (lts--point-from-position position)))

(defun lts--point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.
Prefer `lts-point-from-position', unless there's a real performance bottleneck.

This function must be called within a `lts--save-context' block."
  (goto-char position)
  (let ((line-number (line-number-at-pos position))
        ;; TODO: Add tests that fail if `current-column' is used instead.
        (byte-column (- (position-bytes position)
                        (position-bytes (line-beginning-position)))))
    (cons line-number byte-column)))

(defun lts-point-to-position (point)
  "Convert tree-sitter POINT to buffer position.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See
`lts-parse-chunks' for a more detailed explanation."
  (lts--save-context
    (let ((line-number (car point))
          (byte-column (cdr point)))
      (goto-char 1)
      (forward-line (- line-number 1))
      (byte-to-position (+ byte-column (position-bytes (line-beginning-position)))))))


;;; Extracting buffer's text.

(defun lts--buffer-input (bytepos _line-number _byte-column)
  "Return a portion of the current buffer's text, starting from BYTEPOS.
BYTEPOS is automatically clamped to the range valid for the current buffer.

This function must be called with narrowing disabled, e.g. within a
`lts--without-restriction' block."
  (let* ((max-pos (point-max))
         (beg-byte (max 1 bytepos))
         ;; ;; TODO: Don't hard-code read length.
         (end-byte (+ 1024 beg-byte))
         ;; nil means > max-pos, since we already made sure they are non-negative.
         (beg-pos (or (byte-to-position beg-byte) max-pos))
         (end-pos (or (byte-to-position end-byte) max-pos)))
    (buffer-substring-no-properties beg-pos end-pos)))

(defun lts--buffer-substring-no-properties (beg-byte end-byte)
  "Return the current buffer's text from BEG-BYTE to END-BYTE.
This function must be called with narrowing disabled, e.g. within a
`lts--without-restriction' block."
  (buffer-substring-no-properties
   (byte-to-position beg-byte)
   (byte-to-position end-byte)))

(defun lts--node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree.
Prefer `lts-node-text', unless there's a real bottleneck.

This function must be called with narrowing disabled, e.g. within a
`lts--without-restriction' block."
  (pcase-let ((`(,beg . ,end) (lts-node-position-range node)))
    (buffer-substring-no-properties beg end)))

(defun lts-node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree."
  (lts--without-restriction
    (lts--node-text node)))


;;; Convenient versions of some functions.

(defun lts-get-descendant-for-position-range (node beg end)
  "Return the smallest node within NODE that spans the range (BEG . END)."
  (lts-get-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun lts-get-named-descendant-for-position-range (node beg end)
  "Return the smallest named node within NODE that spans the range (BEG . END)."
  (lts-get-named-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun lts-get-child-by-field (node field)
  "Return NODE's child associated with FIELD, which should be a keyword."
  (unless (keywordp field)
    (signal 'wrong-type-argument (list 'keywordp field)))
  (lts--get-child-by-field-name node (substring (symbol-name field) 1)))

(defun lts-node-start-position (node)
  "Return NODE's start position."
  (byte-to-position (lts-node-start-byte node)))

(defun lts-node-end-position (node)
  "Return NODE's end position."
  (byte-to-position (lts-node-end-byte node)))

(defun lts-node-position-range (node)
  "Return NODE's (START-POSITION . END-POSITION)."
  (let ((range (lts-node-byte-range node)))
    (cl-callf byte-to-position (car range))
    (cl-callf byte-to-position (cdr range))
    range))

(defun lts-goto-first-child-for-position (cursor position)
  "Move CURSOR to the first child that extends beyond the given POSITION.
Return the index of the child node if one was found, nil otherwise."
  (lts-goto-first-child-for-byte cursor (position-bytes position)))

(defun lts-lang-field-id (language field)
  "Return the numeric id of FIELD in LANGUAGE. FIELD should be a keyword."
  (unless (keywordp field)
    (signal 'wrong-type-argument (list 'keywordp field)))
  (lts--lang-field-id-for-name language (substring (symbol-name field) 1)))

(defun lts-lang-node-type-id (language node-type)
  "Return the numeric id of NODE-TYPE in LANGUAGE.
NODE-TYPE should be a symbol (named nodes) or a string (anonymous nodes)."
  (cond
   ((symbolp node-type)
    (lts--lang-type-id-for-name language (symbol-name node-type) :named))
   (t
    (lts--lang-type-id-for-name language node-type nil))))


;;; Querying.

(defun lts--stringify-patterns (patterns)
  "Convert PATTERNS into a query string that can be passed to `lts--make-query'."
  (cond
   ((stringp patterns) patterns)
   ((sequencep patterns)
    ;; XXX: This is hacky.
    (thread-last (mapconcat (lambda (p) (format "%S" p)) patterns "\n")
      (replace-regexp-in-string (regexp-quote "\\?") "?")
      (replace-regexp-in-string (regexp-quote "\\.") ".")))
   (t (error "Invalid patterns"))))

(defun lts-make-query (language patterns &optional tag-assigner)
  "Create a new query for LANGUAGE from a sequence of S-expression PATTERNS.
The query is associated with LANGUAGE, and can only be run on syntax nodes
parsed with LANGUAGE.

When the query is executed, each captured node is tagged with a symbol, whose
name is the corresponding capture name defined in PATTERNS. For example, nodes
that are captured as \"@function.builtin\" will be tagged with the symbol
`function.builtin'. This behavior can be customized by the optional function
TAG-ASSIGNER, which should return a tag value when given a capture name (without
the prefix \"@\"). If it returns nil, the associated capture name is disabled.

See also: `lts-query-captures' and `lts-query-matches'."
  (lts--make-query language (lts--stringify-patterns patterns)
                  (or tag-assigner #'intern)))

(defun lts-query-matches (query node text-function &optional cursor)
  "Execute QUERY on NODE and return a sequence of matches.
Matches are sorted in the order they were found.

Each match has the form (PATTERN-INDEX . MATCH-CAPTURES), where PATTERN-INDEX is
the 0-based position of the matched pattern within QUERY, and MATCH-CAPTURES is
a sequence of captures associated with the match, similar to that returned by
`lts-query-captures'.

TEXT-FUNCTION is called to get nodes' texts (for text-based predicates). It
should take 2 parameters: (BEG-BYTE END-BYTE), and return the corresponding
chunk of text in the source code.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise, a newly created query-cursor is used."
  (lts--query-cursor-matches
   (or cursor (lts-make-query-cursor)) query node text-function))

(defun lts-query-captures (query node text-function &optional cursor)
  "Execute QUERY on NODE and return a sequence of captures.
Captures are sorted in the order they appear.

Each capture has the form (CAPTURE-TAG . CAPTURED-NODE), where CAPTURE-TAG is a
symbol, whose name is the corresponding capture name defined in QUERY (without
the prefix \"@\"). If QUERY was created with a custom tag assigner, CAPTURE-TAG
is the value returned by that function instead. See also: `lts-make-query'.

TEXT-FUNCTION is called to get nodes' texts (for text-based predicates). It
should take 2 parameters: (BEG-BYTE END-BYTE), and return the corresponding
chunk of text in the source code.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise, a newly created query-cursor is used."
  (lts--query-cursor-captures
   (or cursor (lts-make-query-cursor)) query node text-function))


;;; Utilities.

(defun lts-pp-to-string (tree)
  "Return the pretty-printed string of TREE's sexp."
  (pp-to-string (read (lts-tree-to-sexp tree))))

(defun lts--node-steps (node)
  "Return the sequence of steps from the root node to NODE.

Each step has the form (CHILD-NODE . NTH), where CHILD-NODE is the node to
descend into, and NTH is its 0-based ordinal position within the parent node.

If NODE is the root node, the sequence is empty."
  (let ((steps)
        (parent)
        (this node))
    (while (setq parent (lts-get-parent this))
      (push (catch :lts-step
              (let ((i 0))
                (lts-mapc-children (lambda (child)
                                    (if (lts-node-eq child this)
                                        (throw :lts-step (cons this i))
                                      (setq i (1+ i))))
                                  parent))
              (throw :lts-is-not-parents-child (cons this parent)))
            steps)
      (setq this parent))
    steps))

(define-error 'lts--invalid-node-step "Cannot follow node step")

(defun lts--node-from-steps (tree steps)
  "Follow STEPS from TREE's root node; return the final node.
STEPS should be a sequence of steps, as described by `lts--node-steps'.

If a step cannot be followed, signal a `lts--invalid-node-step' error."
  (let ((this (lts-root-node tree)))
    (pcase-dolist (`(,old-node . ,i) steps)
      (let ((new-node (lts-get-nth-child this i)))
        (unless new-node
          (signal 'lts--invalid-node-step (list this old-node i new-node)))
        (let ((new-type (lts-node-type new-node))
              (old-type (lts-node-type old-node)))
          (unless (equal old-type new-type)
            (signal 'lts--invalid-node-step (list this old-node i new-node))))
        (setq this new-node)))
    this))

(provide 'lts)
;;; lts.el ends here
