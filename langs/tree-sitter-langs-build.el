;;; tree-sitter-langs-build.el --- Building grammar bundle -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains functions to obtain and build `tree-sitter' grammars.

;;; Code:

(require 'seq)
(require 'pp)
(require 'dired-aux)
(require 'url)
(require 'tar-mode)

(eval-when-compile
  (require 'subr-x)
  (require 'pcase)
  (require 'cl-lib))

(declare-function dired-omit-mode "dired-x" (&optional arg))
(declare-function magit-get-current-tag "magit-git" (&optional rev with-distance))

(defconst tree-sitter-langs--suffixes '(".dylib" ".dll" ".so")
  "List of suffixes for shared libraries that define tree-sitter languages.")

(defconst tree-sitter-langs--dir
  (file-name-directory (locate-library "tree-sitter-langs")))

(defconst tree-sitter-langs--bin-dir
  (file-name-as-directory
   (concat tree-sitter-langs--dir "bin")))

(defconst tree-sitter-langs--version
  (let ((main-file (locate-library "tree-sitter-langs.el")))
    (unless main-file
      (error "Could not find tree-sitter-langs.el"))
    (with-temp-buffer
      (insert-file-contents main-file)
      (unless (re-search-forward ";; Version: \\(.+\\)")
        (error "Could not determine tree-sitter-langs version"))
      (match-string 1))))

(defconst tree-sitter-langs--os
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ (error "Unsupported system-type %s" system-type))))

(defun tree-sitter-langs--bundle-file (&optional ext version os)
  "Return the grammar bundle file's name, with optional EXT.
If VERSION and OS are not spcified, use the defaults of
`tree-sitter-langs--version' and `tree-sitter-langs--os'."
  (format "tree-sitter-grammars-%s-%s.tar%s"
          (or os tree-sitter-langs--os)
          (or version tree-sitter-langs--version)
          (or ext "")))

(defun tree-sitter-langs--bundle-url (&optional version os)
  "Return the URL to download the grammar bundle.
If VERSION and OS are not spcified, use the defaults of
`tree-sitter-langs--version' and `tree-sitter-langs--os'."
  (format "https://dl.bintray.com/ubolonton/emacs/%s"
          (tree-sitter-langs--bundle-file ".gz" version os)))

;;; A list of (LANG-SYMBOL VERSION-TO-BUILD &optional PATHS REPO-URL).
(defconst tree-sitter-langs-repos
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
    (rust       "3e5ec5a")
    (scala      "v0.13.0")
    (swift      "a22fa5e")
    (typescript "v0.16.1" ("typescript" "tsx")))
  "List of language symbols and their corresponding grammar sources.")

(defconst tree-sitter-langs--repos-dir
  (file-name-as-directory
   (concat (file-name-directory (locate-library "tree-sitter-langs"))
           "repos"))
  "Directory to store grammar repos, for compilation.")

(defun tree-sitter-langs--source (lang-symbol)
  "Return a plist describing the source of the grammar for LANG-SYMBOL."
  (when-let ((source (alist-get lang-symbol tree-sitter-langs-repos)))
    (let ((version (or (nth 0 source) "origin/master"))
          (paths (or (nth 1 source) (list "")))
          (repo (or (nth 2 source) (format "https://github.com/tree-sitter/tree-sitter-%s" (symbol-name lang-symbol)))))
      (list :repo repo :version version :paths paths))))

(defvar tree-sitter-langs--out nil)

;;; TODO: Use (maybe make) an async library, with a proper event loop, instead
;;; of busy-waiting.
(defun tree-sitter-langs--call (program &rest args)
  "Call PROGRAM with ARGS, using BUFFER as stdout+stderr.
If BUFFER is nil, `princ' is used to forward its stdout+stderr."
  (let* ((command `(,program . ,args))
         (_ (message "[tree-sitter-langs] Running %s in %s" command default-directory))
         (base `(:name ,program :command ,command))
         (output (if tree-sitter-langs--out
                     `(:buffer ,tree-sitter-langs--out)
                   `(:filter (lambda (proc string)
                               (princ string)))))
         (proc (let ((process-environment (cons (format "TREE_SITTER_DIR=%s" tree-sitter-langs--dir)
                                                process-environment)))
                 (apply #'make-process (append base output))))
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
          (default-directory (concat tree-sitter-langs--repos-dir
                                     (format "tree-sitter-%s" lang-name))))
       `(,lang-symbol ,(or (magit-get-current-tag "origin/master")
                           "origin/master"))))
   tree-sitter-langs-repos))

(defun tree-sitter-langs--buffer (name)
  "Return a buffer from NAME, as the DESTINATION of `call-process'.
In batch mode, return nil, so that stdout is used instead."
  (unless noninteractive
    (let ((buf (get-buffer-create name)))
      (pop-to-buffer buf)
      (delete-region (point-min) (point-max))
      (redisplay)
      buf)))

(defun tree-sitter-langs-compile (lang-symbol)
  "Download and compile the grammar for LANG-SYMBOL.
This function requires git and tree-sitter CLI."
  (message "[tree-sitter-langs] Processing %s" lang-symbol)
  (unless (executable-find "git")
    (error "Could not find git (needed to download grammars)"))
  (unless (executable-find "tree-sitter")
    (error "Could not find tree-sitter executable (needed to compile grammars)"))
  (let* ((source (tree-sitter-langs--source lang-symbol))
         (lang-name (symbol-name lang-symbol))
         (dir (if source
                  (file-name-as-directory
                   (concat tree-sitter-langs--repos-dir
                           (format "tree-sitter-%s" lang-name)))
                (error "Unknown language `%s'" lang-name)))
         (repo (plist-get source :repo))
         (paths (plist-get source :paths))
         (version (plist-get source :version))
         (tree-sitter-langs--out (tree-sitter-langs--buffer
                                  (format "*tree-sitter-langs-compile %s*" lang-name))))
    (if (file-directory-p dir)
        (let ((default-directory dir))
          (tree-sitter-langs--call "git" "remote" "-v" "update"))
      (tree-sitter-langs--call "git" "clone" "-q" repo dir))
    (let ((default-directory dir))
      (tree-sitter-langs--call "git" "reset" "--hard" version)
      (tree-sitter-langs--call "npm" "set" "progress=false")
      ;; TODO: Figure out why we need to skip `npm install' for some repos.
      (ignore-errors
        (tree-sitter-langs--call "npm" "install"))
      ;; A repo can have multiple grammars (e.g. typescript + tsx).
      (dolist (path paths)
        (let ((default-directory (file-name-as-directory (concat dir path))))
          (tree-sitter-langs--call "tree-sitter" "generate")
          (tree-sitter-langs--call "tree-sitter" "test")))
      ;; On macOS, rename .so => .dylib, because we will make a "universal"
      ;; bundle.
      (when (eq system-type 'darwin)
        ;; This renames existing ".so" files as well.
        (let ((default-directory tree-sitter-langs--bin-dir))
          (dolist (file (directory-files default-directory))
            (when (string-suffix-p ".so" file)
              (let ((new-name (concat (file-name-base file) ".dylib")))
                (when (file-exists-p new-name)
                  (delete-file new-name))
                (rename-file file new-name))))))
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
                       (condition-case err
                           (tree-sitter-langs-compile lang-symbol)
                         (error `[,lang-symbol ,err])))))
                  (seq-filter #'identity))))
    (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    (unwind-protect
        (let* ((tar-file (concat (file-name-as-directory
                                  (expand-file-name default-directory))
                                 (tree-sitter-langs--bundle-file) ".gz"))
               (default-directory tree-sitter-langs--bin-dir)
               (tree-sitter-langs--out (tree-sitter-langs--buffer "*tree-sitter-langs-create-bundle*"))
               (files (seq-filter (lambda (file)
                                    (when (seq-some (lambda (ext) (string-suffix-p ext file))
                                                    tree-sitter-langs--suffixes)
                                      file))
                                  (directory-files default-directory)))
               ;; Disk names in Windows can confuse tar, so we need this option. BSD
               ;; tar (macOS) doesn't have it, so we don't set it everywhere.
               ;; https://unix.stackexchange.com/questions/13377/tar/13381#13381.
               (tar-opts (pcase system-type
                           ('windows-nt '("--force-local")))))
          (apply #'tree-sitter-langs--call "tar" "-zcvf" tar-file (append tar-opts files)))
      (when errors
        (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        (display-warning 'tree-sitter
                         (format "Could not compile grammars:\n%s" (pp-to-string errors)))))))

;;;###autoload
(defun tree-sitter-langs-install (&optional version os keep-bundle)
  "Download and install the specified VERSION of the language grammar bundle.
If VERSION and OS are not specified, use the defaults of
`tree-sitter-langs--version' and `tree-sitter-langs--os'.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil."
  (interactive)
  (let ((dir tree-sitter-langs--bin-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((default-directory dir)
          (bundle-file (tree-sitter-langs--bundle-file ".gz" version os)))
      ;; FIX: Handle HTTP errors properly.
      (url-copy-file (tree-sitter-langs--bundle-url version os)
                     bundle-file 'ok-if-already-exists)
      (with-temp-buffer
        (insert-file-contents bundle-file)
        (tar-mode)
        (tar-untar-buffer))
      (unless keep-bundle
        (delete-file bundle-file 'trash))
      (when (y-or-n-p (format "Show installed grammars in %s? " dir))
        (with-current-buffer (find-file dir)
          (when (bound-and-true-p dired-omit-mode)
            (dired-omit-mode -1)))))))

;;; This doesn't actually belong here, but for convenience we don't want to put
;;; this in another `tree-sitter-bootstrap' module.
(defun tree-sitter-download-dyn-module ()
  "Download the pre-compiled `tree-sitter-dyn' module."
  (let* ((main-file (locate-library "tree-sitter.el"))
         (_ (unless main-file
              (error "Could not find tree-sitter.el")))
         (version (with-temp-buffer
                    (insert-file-contents main-file)
                    (unless (re-search-forward ";; Version: \\(.+\\)")
                      (error "Could not determine tree-sitter version"))
                    (match-string 1)))
         (ext (pcase system-type
                ('windows-nt "dll")
                ('darwin "dylib")
                ('gnu/linux "so")
                (_ (error "Unsupported system-type %s" system-type))))
         (dyn-file (format "tree-sitter-dyn.%s" ext))
         (gz-file (format "%s.gz" dyn-file))
         (url (format "https://github.com/ubolonton/emacs-tree-sitter/releases/download/%s/%s"
                      version gz-file))
         (default-directory (file-name-directory main-file)))
    (if (file-exists-p dyn-file)
        (when (y-or-n-p (format "Overwrite %s? " dyn-file))
          (url-copy-file url gz-file)
          (delete-file dyn-file)
          (dired-compress-file gz-file))
      (url-copy-file url gz-file)
      ;; FIX: Uncompressing with `dired-compress-file' doesn't work on Windows.
      (dired-compress-file gz-file))))

(provide 'tree-sitter-langs-build)
;;; tree-sitter-langs-build.el ends here
