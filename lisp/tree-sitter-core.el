;;; tree-sitter-core.el --- Core tree-sitter APIs -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;;         Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>

;;; Commentary:

;; This file contains the core functionalities of tree-sitter.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

;; Load the dynamic module at compile time as well, to satisfy the byte compiler.
(eval-and-compile
  ;; XXX: We want a universal package containing binaries for all platforms, so we use a unique
  ;; extension for each. On macOS, we use`.dylib', which is more sensible than `.so' anyway.
  (when (eq system-type 'darwin)
    (load "tree-sitter--mac-load.el")))

;; We still call this on macOS, as it's useful for other things as well.
(require 'tree-sitter-dyn)

(eval-when-compile
  (require 'pcase))

(defmacro ts--without-restriction (&rest body)
  "Execute BODY with narrowing disabled."
  (declare (indent 0))
  `(save-restriction
     (widen)
     ,@body))

(defmacro ts--save-context (&rest body)
  "Execute BODY wrapped in a `save-excursion', with narrowing disabled."
  (declare (indent 0))
  `(save-excursion
     (ts--without-restriction
       ,@body)))


;;; Type conversion.

(defun ts-point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See `ts-parse-chunks'
for a more detailed explanation."
  (ts--save-context
    (ts--point-from-position position)))

(defun ts--point-from-position (position)
  "Convert POSITION to a valid tree-sitter point.
Prefer `ts-point-from-position', unless there's a real performance bottleneck.

This function must be called within a `ts--save-context' block."
  (goto-char position)
  (let ((line-number (line-number-at-pos position))
        ;; TODO: Add tests that fail if `current-column' is used instead.
        (byte-column (- (position-bytes position)
                        (position-bytes (line-beginning-position)))))
    (cons line-number byte-column)))

(defun ts-point-to-position (point)
  "Convert tree-sitter POINT to buffer position.

A \"point\" in this context is a (LINE-NUMBER . BYTE-COLUMN) pair. See `ts-parse-chunks'
for a more detailed explanation."
  (ts--save-context
    (let ((line-number (car point))
          (byte-column (cdr point)))
      (goto-char 1)
      (forward-line (- line-number 1))
      (byte-to-position (+ byte-column (position-bytes (line-beginning-position)))))))


;;; Extracting buffer's text.

(defun ts-buffer-input (bytepos _line-number _byte-column)
  "Return a portion of the current buffer's text, starting from BYTEPOS.
BYTEPOS is automatically clamped to the range valid for the current buffer.

This function must be called with narrowing disabled, e.g. within a
`ts--without-restriction' block."
  (let* ((max-pos (point-max))
         (beg-byte (max 1 bytepos))
         ;; ;; TODO: Don't hard-code read length.
         (end-byte (+ 1024 beg-byte))
         ;; nil means > max-pos, since we already made sure they are non-negative.
         (beg-pos (or (byte-to-position beg-byte) max-pos))
         (end-pos (or (byte-to-position end-byte) max-pos)))
    (buffer-substring-no-properties beg-pos end-pos)))

(defun ts--node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree.
Prefer `ts-node-text', unless there's a real bottleneck.

This function must be called within a `ts--without-restriction' block."
  (pcase-let ((`[,beg ,end] (ts-node-range node)))
    (buffer-substring-no-properties
     (byte-to-position beg)
     (byte-to-position end))))

(defun ts-node-text (node)
  "Return NODE's text, assuming it's from the current buffer's syntax tree."
  (ts--without-restriction
    (ts--node-text node)))


;;; Convenient versions of some functions.

(defun ts-get-descendant-for-position-range (node beg end)
  "Return the smallest node within NODE that spans the position range [BEG END]."
  (ts-get-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun ts-get-named-descendant-for-position-range (node beg end)
  "Return the smallest named node within NODE that spans the position range [BEG END]."
  (ts-get-named-descendant-for-byte-range
   node
   (position-bytes beg)
   (position-bytes end)))

(defun ts-node-start-position (node)
  "Return NODE's start position."
  (byte-to-position (ts-node-start-byte node)))

(defun ts-node-end-position (node)
  "Return NODE's end position."
  (byte-to-position (ts-node-end-byte node)))

(defun ts-node-position-range (node)
  "Return NODE's (START-POSITION . END-POSITION)."
  (pcase-let ((`[,beg ,end] (ts-node-range node)))
    (cons
     (byte-to-position beg)
     (byte-to-position end))))

(defun ts-goto-first-child-for-position (cursor position)
  "Move CURSOR to the first child that extends beyond the given POSITION.
Return the index of the child node if one was found, nil otherwise."
  (ts-goto-first-child-for-byte cursor (position-bytes position)))


;;; Querying.

(defun ts--stringify-patterns (patterns)
  (cond
   ((stringp patterns) patterns)
   ((sequencep patterns)
    ;; XXX: This is hacky.
    (thread-last (mapconcat (lambda (p) (format "%S" p)) patterns "\n")
      (replace-regexp-in-string (regexp-quote "\\?") "?")
      (replace-regexp-in-string (regexp-quote "\\.") ".")))
   (t (error "Invalid patterns"))))

(defun ts-make-query (language patterns)
  "Create a new query for LANGUAGE from a sequence of S-expression PATTERNS.
The query is associated with LANGUAGE, and can only be run on syntax nodes
parsed with LANGUAGE."
  (ts--make-query language (ts--stringify-patterns patterns)))

(defun ts-query-matches (query node &optional cursor index-only text-function)
  "Execute QUERY on NODE and return a sequence of matches.
Matches are sorted in the order they were found.

Each match has the form (PATTERN-INDEX . MATCH-CAPTURES), where PATTERN-INDEX is
the position of the matched pattern within QUERY, and MATCH-CAPTURES is a
sequence of captures associated with the match, similar to that returned by
`ts-query-captures'. If the optional arg INDEX-ONLY is non-nil, positions of the
capture patterns within QUERY are returned instead of their names.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise a new query-cursor is used.

If the optional arg TEXT-FUNCTION is non-nil, it is used to get nodes' text.
Otherwise `ts-node-text' is used."
  (ts--query-cursor-matches
   (or cursor (ts-make-query-cursor)) query node index-only (or text-function #'ts-node-text)))

(defun ts-query-captures (query node &optional cursor index-only text-function)
  "Execute QUERY on NODE and return a sequence of captures.
Matches are sorted in the order they appear.

Each capture has the form (CAPTURE-NAME . CAPTURED-NODE). If the optional arg
INDEX-ONLY is non-nil, the position of the capture pattern within QUERY is
returned instead of its name.

If the optional arg CURSOR is non-nil, it is used as the query-cursor to execute
QUERY. Otherwise a new query-cursor is used.

If the optional arg TEXT-FUNCTION is non-nil, it is used to get nodes' text.
Otherwise `ts-node-text' is used."
  (ts--query-cursor-captures
   (or cursor (ts-make-query-cursor)) query node index-only (or text-function #'ts-node-text)))


;;; Utilities.

(defun ts-pp-to-string (tree)
  "Return the pretty-printed string of TREE's sexp."
  (pp-to-string (read (ts-tree-to-sexp tree))))

(defun ts--node-steps (node)
  "Return the sequence of steps from the root node to NODE.

Each step has the form (CHILD-NODE . NTH), where CHILD-NODE is the node to
descend into, and NTH is its 0-based ordinal position within the parent node.

If NODE is the root node, the sequence is empty."
  (let ((steps)
        (parent)
        (this node))
    (while (setq parent (ts-get-parent this))
      (push (catch :ts-step
              (let ((i 0))
                (ts-mapc-children (lambda (child)
                                    (if (ts-node-eq child this)
                                        (throw :ts-step (cons this i))
                                      (setq i (1+ i))))
                                  parent))
              (throw :ts-is-not-parents-child (cons this parent)))
            steps)
      (setq this parent))
    steps))

(define-error 'ts--invalid-node-step "Cannot follow node step")

(defun ts--node-from-steps (tree steps)
  "Follow STEPS from TREE's root node; return the final node.
STEPS shoud be a sequence of steps, as described by `ts--node-steps'.

If a step cannot be followed, signal a `ts--invalid-node-step' error."
  (let ((this (ts-root-node tree)))
    (pcase-dolist (`(,old-node . ,i) steps)
      (let ((new-node (ts-get-nth-child this i)))
        (unless new-node
          (signal 'ts--invalid-node-step (list this old-node i new-node)))
        (let ((new-type (ts-node-type new-node))
              (old-type (ts-node-type old-node)))
          (unless (equal old-type new-type)
            (signal 'ts--invalid-node-step (list this old-node i new-node))))
        (setq this new-node)))
    this))

(provide 'tree-sitter-core)
;;; tree-sitter-core.el ends here
