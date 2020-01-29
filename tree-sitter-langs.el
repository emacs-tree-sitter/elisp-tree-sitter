;;; tree-sitter-langs.el --- Language definitions for tree-sitter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This is a convenient bundle of language definitions for `tree-sitter'. It
;; serves as an interim distribution mechanism, until `tree-sitter' is
;; widespread enough for language major modes to include the definitions on
;; their own.

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'pcase))

(require 'seq)
(require 'dired-aux)

;; (require 'tree-sitter)

(defvar tree-sitter-langs-repos
  '((agda       "origin/master" "https://github.com/tree-sitter/tree-sitter-agda")
    (bash       "origin/master")
    (c          "origin/master")
    (c-sharp    "origin/master")
    (cpp        "v0.16.0" )
    (css        "origin/master")
    ;; (fluent     "origin/master"
    (go         "origin/master")
    ;; (haskell    "origin/master")
    (html       "origin/master")
    (java       "origin/master")
    (javascript "origin/master")
    (jsdoc      "origin/master")
    (json       "origin/master")
    ;; (julia      "origin/master")
    (ocaml      "origin/master")
    (php        "origin/master")
    (python     "origin/master")
    (ruby       "origin/master")
    (rust       "origin/master")
    ;; (scala      "origin/master")
    (swift      "origin/master")
    ;; (typescript "origin/master")
    )
  "List of language symbols and their corresponding grammar sources.")

(defvar tree-sitter-langs--grammars-dir
  (file-name-as-directory
   (concat (file-name-directory (locate-library "tree-sitter"))
           "grammars")))

(defun tree-sitter-langs--source (lang-symbol)
  "Return a pair of (REPO . VERSION) to download grammar for LANG-SYMBOL from."
  (when-let ((source (alist-get lang-symbol tree-sitter-langs-repos)))
    (let ((version (or (car source) "origin/master"))
          (repo (or (cadr source) (format "https://github.com/tree-sitter/tree-sitter-%s" (symbol-name lang-symbol)))))
      (cons repo version))))

(defun tree-sitter-langs--call (program out &rest args)
  "Call PROGRAM with ARGS, using OUT as stdout."
  (message "[tree-sitter-langs] Running %s %s" program args)
  (let ((exit-code (apply #'call-process program nil out 'display args)))
    (unless (= exit-code 0)
      (error "%s %s exited with code %s" program args exit-code))))

(defun tree-sitter-langs--buf-or-stdout (name)
  "Return a buffer from NAME, as the DESTINATION of `call-process'.
In batch mode, return stdout."
  (if noninteractive
      '(:file "/dev/stdout")
    (let ((buf (get-buffer-create name)))
      (pop-to-buffer buf)
      (delete-region (point-min) (point-max))
      (redisplay)
      buf)))

(defun ts--get-cli-directory ()
  "Return tree-sitter CLI's directory, including the ending separator.
This is the directory where the CLI tool keeps compiled lang definitions, among
other data."
  (file-name-as-directory
   (expand-file-name
    ;; https://github.com/tree-sitter/tree-sitter/blob/1bad6dc/cli/src/config.rs#L20
    (if-let ((dir (getenv "TREE_SITTER_DIR")))
        dir
      "~/.tree-sitter"))))

(defvar ts--language-grammar-ext
  (pcase system-type
    ((or 'darwin 'gnu/linux) ".so")
    ('windows-nt ".dll")
    (_ (error "Unsupported system-type %s" system-type))))

;;; TODO: Load to check binary compatibility.
(defun tree-sitter-langs-compile (lang-symbol)
  "Download and compile the grammar for LANG-SYMBOL.
Requires git and tree-sitter CLI."
  (unless (executable-find "git")
    (error "Could not find git (needed to download grammars)"))
  (unless (executable-find "tree-sitter")
    (error "Could not find tree-sitter executable (needed to compile grammars)"))
  (unless (executable-find "tar")
    (error "Could not find tar executable (needed to bundle compiled grammars)"))
  (let* ((source (tree-sitter-langs--source lang-symbol))
         (lang-name (symbol-name lang-symbol))
         (dir (if source
                  (concat tree-sitter-langs--grammars-dir
                          (format "tree-sitter-%s" lang-name))
                (error "Unknown language `%s'" lang-name)))
         (repo (car source))
         (version (cdr source))
         (out (tree-sitter-langs--buf-or-stdout
               (format "*tree-sitter-langs-compile %s*" lang-name))))
    (if (file-directory-p dir)
        (let ((default-directory dir))
          (tree-sitter-langs--call "git" out "remote" "-v" "update"))
      (tree-sitter-langs--call "git" out "clone" "-v" repo dir))
    (let ((default-directory dir))
      (tree-sitter-langs--call "git" out "reset" "--hard" version)
      (tree-sitter-langs--call "npm" out "install")
      (tree-sitter-langs--call "tree-sitter" out "generate")
      (tree-sitter-langs--call "tree-sitter" out "test")
      (tree-sitter-langs--call "git" out "reset" "--hard" "HEAD")
      (tree-sitter-langs--call "git" out "clean" "-f"))))

(defun tree-sitter-langs-create-bundle ()
  "Create a bundle of language grammars.
The bundle includes all languages declared in `tree-sitter-langs-repos'."
  (pcase-dolist (`(,lang-symbol . _) tree-sitter-langs-repos)
    (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (message "[tree-sitter-langs] Processing %s" lang-symbol)
    (tree-sitter-langs-compile lang-symbol))
  (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  (let* ((tar-file (concat (file-name-as-directory
                            (expand-file-name default-directory))
                           "tree-sitter-langs.tar"))
         (default-directory (file-name-as-directory
                             (expand-file-name (concat (ts--get-cli-directory) "bin"))))
         (out (tree-sitter-langs--buf-or-stdout "*tree-sitter-langs-create-bundle*"))
         (files (seq-filter (lambda (file)
                              (when (string-suffix-p ts--language-grammar-ext file)
                                file))
                            (directory-files default-directory))))
    (apply #'tree-sitter-langs--call "tar" out "-cvf" tar-file files)
    (dired-compress-file tar-file)))

(defun tree-sitter-langs-install ()
  "Download and install the language grammar bundle."
  TODO)

(provide 'tree-sitter-langs)
;;; tree-sitter-langs.el ends here
