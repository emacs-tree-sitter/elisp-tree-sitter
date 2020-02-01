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

(require 'seq)
(require 'pp)
(require 'dired-aux)

(require 'tree-sitter-cli)

(eval-when-compile
  (require 'subr-x)
  (require 'pcase))

(defconst tree-sitter-langs--version "0.0.5")

(defconst tree-sitter-langs--os
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ (error "Unsupported system-type %s" system-type))))

(defconst tree-sitter-langs--bundle-file
  (format "tree-sitter-grammars-%s-%s.tar"
          tree-sitter-langs--os
          tree-sitter-langs--version))

(defconst tree-sitter-langs--bundle-url
  (format "https://dl.bintray.com/ubolonton/emacs-tree-sitter/%s.gz"
          tree-sitter-langs--bundle-file))

;;; A list of (LANG-SYMBOL VERSION-TO-BUILD &optional REPO-URL).
(defvar tree-sitter-langs-repos
  '((agda       "v1.2.1")
    (bash       "v0.16.0")
    (c          "v0.16.0")
    (c-sharp    "v0.16.1")
    (cpp        "v0.16.0")
    (css        "v0.16.0")
    (fluent     "v0.12.0")
    (go         "v0.16.0")
    (haskell    "v0.13.0")
    (html       "v0.16.0")
    (java       "v0.16.0")
    (javascript "v0.16.0")
    (jsdoc      "v0.16.0")
    (json       "v0.16.0")
    (julia      "v0.0.3")
    (ocaml      "v0.15.0")
    (php        "v0.16.1")
    (python     "v0.16.0")
    (ruby       "v0.16.1")
    (rust       "v0.16.0")
    (scala      "v0.13.0")
    (swift      "a22fa5e")
    (typescript "v0.16.1"))
  "List of language symbols and their corresponding grammar sources.")

(defconst tree-sitter-langs--grammars-dir
  (file-name-as-directory
   (concat (file-name-directory (locate-library "tree-sitter"))
           "grammars"))
  "Directory to store grammar repos, for compilation.")

(defun tree-sitter-langs--source (lang-symbol)
  "Return a pair of (REPO . VERSION) to download grammar for LANG-SYMBOL from."
  (when-let ((source (alist-get lang-symbol tree-sitter-langs-repos)))
    (let ((version (or (car source) "origin/master"))
          (repo (or (cadr source) (format "https://github.com/tree-sitter/tree-sitter-%s" (symbol-name lang-symbol)))))
      (cons repo version))))

(defvar tree-sitter-langs--out nil)

;;; TODO: Use (maybe make) an async library, with a proper event loop, instead
;;; of busy-waiting.
(defun tree-sitter-langs--call (program &rest args)
  "Call PROGRAM with ARGS, using BUFFER as stdout+stderr.
If BUFFER is nil, `princ' is used to forward its stdout+stderr."
  (let* ((command `(,program . ,args))
         (_ (message "[tree-sitter-langs] Running %s" command))
         (base `(:name ,program :command ,command))
         (output (if tree-sitter-langs--out
                     `(:buffer ,tree-sitter-langs--out)
                   `(:filter (lambda (proc string)
                               (princ string)))))
         (proc (apply #'make-process (append base output)))
         (exit-code (progn
                      (while (not (memq (process-status proc)
                                        '(exit failed signal)))
                        (sleep-for 0.1))
                      (process-exit-status proc))))
    (unless (= exit-code 0)
      (error "Error calling %s, exit code is %s" command exit-code))))

(defun tree-sitter-langs--get-latest-tags ()
  "Return the `tree-sitter-langs-repos' with versions replaced by latest tags.
If there's no tag, return \"origin/master\"."
  (require 'magit)
  (seq-map
   (lambda (desc)
     (pcase-let*
         ((`(,lang-symbol . _) desc)
          (lang-name (symbol-name lang-symbol))
          (default-directory (concat tree-sitter-langs--grammars-dir
                                     (format "tree-sitter-%s" lang-name))))
       `(,lang-symbol ,(or (magit-get-current-tag)
                           "origin/master"))))
   tree-sitter-langs-repos))

(defun tree-sitter-langs--buffer (name)
  "Return a buffer from NAME, as the DESTINATION of `call-process'.
In batch mode, return stdout."
  (unless noninteractive
    (let ((buf (get-buffer-create name)))
      (pop-to-buffer buf)
      (delete-region (point-min) (point-max))
      (redisplay)
      buf)))

;;; TODO: Load to check binary compatibility.
(defun tree-sitter-langs-compile (lang-symbol)
  "Download and compile the grammar for LANG-SYMBOL.
Requires git and tree-sitter CLI."
  (unless (executable-find "git")
    (error "Could not find git (needed to download grammars)"))
  (unless (executable-find "tree-sitter")
    (error "Could not find tree-sitter executable (needed to compile grammars)"))
  (let* ((source (tree-sitter-langs--source lang-symbol))
         (lang-name (symbol-name lang-symbol))
         (dir (if source
                  (concat tree-sitter-langs--grammars-dir
                          (format "tree-sitter-%s" lang-name))
                (error "Unknown language `%s'" lang-name)))
         (repo (car source))
         (version (cdr source))
         (tree-sitter-langs--out (tree-sitter-langs--buffer
                                  (format "*tree-sitter-langs-compile %s*" lang-name))))
    (if (file-directory-p dir)
        (let ((default-directory dir))
          (tree-sitter-langs--call "git" "remote" "-v" "update"))
      (tree-sitter-langs--call "git" "clone" "-v" repo dir))
    (let ((default-directory dir))
      (tree-sitter-langs--call "git" "reset" "--hard" version)
      ;; TODO: Figure out why we need to skip `npm install' for some repos.
      (ignore-errors
        (tree-sitter-langs--call "npm" "install"))
      (tree-sitter-langs--call "tree-sitter" "generate")
      (tree-sitter-langs--call "tree-sitter" "test")
      (tree-sitter-langs--call "git" "reset" "--hard" "HEAD")
      (tree-sitter-langs--call "git" "clean" "-f"))))

(defun tree-sitter-langs-create-bundle ()
  "Create a bundle of language grammars.
The bundle includes all languages declared in `tree-sitter-langs-repos'."
  (unless (executable-find "tar")
    (error "Could not find tar executable (needed to bundle compiled grammars)"))
  (let ((errors (thread-last tree-sitter-langs-repos
                  (seq-map
                   (lambda (desc)
                     (pcase-let ((`(,lang-symbol . _) desc))
                       (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                       (message "[tree-sitter-langs] Processing %s" lang-symbol)
                       (condition-case err
                           (tree-sitter-langs-compile lang-symbol)
                         (error `[,lang-symbol ,err])))))
                  (seq-filter #'identity))))
    (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (let* ((tar-file (concat (file-name-as-directory
                              (expand-file-name default-directory))
                             tree-sitter-langs--bundle-file))
           (default-directory (tree-sitter-cli-bin-directory))
           (tree-sitter-langs--out (tree-sitter-langs--buffer "*tree-sitter-langs-create-bundle*"))
           (files (seq-filter (lambda (file)
                                (when (string-suffix-p tree-sitter-cli-compiled-grammar-ext file)
                                  file))
                              (directory-files default-directory)))
           ;; Disk names in Windows can confuse tar, so we need this option. BSD
           ;; tar (macOS) doesn't have it, so we don't set it everywhere.
           ;; https://unix.stackexchange.com/questions/13377/tar/13381#13381.
           (tar-opts (pcase system-type
                       ('windows-nt '("--force-local")))))
      (apply #'tree-sitter-langs--call "tar" "-cvf" tar-file (append tar-opts files))
      (let ((dired-compress-file-suffixes '(("\\.tar\\'" ".tar.gz" nil))))
        (dired-compress-file tar-file)))
    (when errors
      (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (display-warning 'tree-sitter
                       (format "Could not compile grammars:\n%s" (pp-to-string errors))))))

(defun tree-sitter-langs-install ()
  "Download and install the language grammar bundle."
  TODO)

(provide 'tree-sitter-langs)
;;; tree-sitter-langs.el ends here
