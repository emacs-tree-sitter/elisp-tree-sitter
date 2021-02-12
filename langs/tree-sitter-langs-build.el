;;; tree-sitter-langs-build.el --- Building grammar bundle -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains utilities to obtain and build `tree-sitter' grammars.

;;; Code:

(require 'seq)
(require 'pp)
(require 'url)
(require 'tar-mode)

(eval-when-compile
  (require 'subr-x)
  (require 'pcase)
  (require 'cl-lib))

(declare-function dired-omit-mode "dired-x" (&optional arg))
(declare-function magit-get-current-tag "magit-git" (&optional rev with-distance))
(declare-function magit-rev-parse "magit-git" (&rest args))

(defconst tree-sitter-langs--suffixes '(".dylib" ".dll" ".so")
  "List of suffixes for shared libraries that define tree-sitter languages.")

(defconst tree-sitter-langs--dir
  (file-name-directory (locate-library "tree-sitter-langs.el")))

(defvar tree-sitter-langs--bin-dir nil)

(defcustom tree-sitter-langs-grammar-dir tree-sitter-langs--dir
  "Directory to store grammar binaries."
  :group 'tree-sitter-langs
  :type 'directory
  :set (lambda (sym val)
         (setq tree-sitter-langs--bin-dir
               (concat (file-name-as-directory val) "bin/"))
         (set-default sym val)))

(defconst tree-sitter-langs--queries-dir
  (file-name-as-directory
   (concat tree-sitter-langs--dir "queries")))

(defconst tree-sitter-langs--bundle-version "0.9.2"
  "Version of the grammar bundle.
This is bumped whenever `tree-sitter-langs-repos' is updated, which should be
infrequent (grammar-only changes). It is different from the version of
`tree-sitter-langs', which can change frequently (when queries change).")

(defconst tree-sitter-langs--bundle-version-file "BUNDLE-VERSION")

(defconst tree-sitter-langs--os
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ (error "Unsupported system-type %s" system-type))))

(defun tree-sitter-langs--bundle-file (&optional ext version os)
  "Return the grammar bundle file's name, with optional EXT.
If VERSION and OS are not spcified, use the defaults of
`tree-sitter-langs--bundle-version' and `tree-sitter-langs--os'."
  (format "tree-sitter-grammars-%s-%s.tar%s"
          (or os tree-sitter-langs--os)
          (or version tree-sitter-langs--bundle-version)
          (or ext "")))

(defun tree-sitter-langs--bundle-url (&optional version os)
  "Return the URL to download the grammar bundle.
If VERSION and OS are not specified, use the defaults of
`tree-sitter-langs--bundle-version' and `tree-sitter-langs--os'."
  ;; TODO: Use https://elpa.ubolonton.org/packages/bin as the canonical source.
  (format "https://dl.bintray.com/ubolonton/emacs/%s"
          (tree-sitter-langs--bundle-file ".gz" version os)))

;;; A list of (LANG-SYMBOL VERSION-TO-BUILD &optional PATHS REPO-URL).
(defconst tree-sitter-langs-repos
  '((agda       "d710ff1")
    (bash       "f226a4b")
    (c          "80b6b74")
    (c-sharp    "aae8ab2")
    (cpp        "5e7476b")
    (css        "23f2cb9")
    (elm        "06a8ff7" nil "https://github.com/razzeee/tree-sitter-elm")
    (fluent     "858fdd6")
    (go         "f5cae4e")
    (html       "92c17db")
    (janet-simple "45418f7" nil "https://codeberg.org/sogaiu/tree-sitter-janet-simple")
    (java       "0b18a22")
    (javascript "6296f90")
    (jsdoc      "77e7785")
    (json       "d3976b2")
    (julia      "165e2ae")
    (ocaml      "9e4f226")
    (php        "7df0460")
    (python     "649e752")
    (ruby       "1ce58f2")
    (rust       "40620bf")
    (scala      "904e2b1")
    (swift      "a22fa5e")
    (typescript "a3a4bec" ("typescript" "tsx")))
  "List of language symbols and their corresponding grammar sources.
Note that these are mostly for the grammars. We treat the queries they include
as references, instead of using them directly for syntax highlighting.

If the grammar is not in an \"official\" repo (i.e. belonging to the
organization https://github.com/tree-sitter), the repo URL must be specified.
For example:
    (hit \"bdeac01\" nil \"https://github.com/dschwen/tree-sitter-hit\")")

(defconst tree-sitter-langs--repos-dir
  (file-name-as-directory
   (concat tree-sitter-langs--dir "repos"))
  "Directory to store grammar repos, for compilation.")

(defun tree-sitter-langs--source (lang-symbol)
  "Return a plist describing the source of the grammar for LANG-SYMBOL."
  (when-let ((source (alist-get lang-symbol tree-sitter-langs-repos)))
    (let ((version (or (nth 0 source) "origin/master"))
          (paths (or (nth 1 source) (list "")))
          (repo (or (nth 2 source) (format "https://github.com/tree-sitter/tree-sitter-%s"
                                           (symbol-name lang-symbol)))))
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
         (proc (let ((process-environment (cons (format "TREE_SITTER_DIR=%s"
                                                        tree-sitter-langs-grammar-dir)
                                                process-environment)))
                 (apply #'make-process (append base output))))
         (exit-code (progn
                      (while (not (memq (process-status proc)
                                        '(exit failed signal)))
                        (sleep-for 0.1))
                      (process-exit-status proc))))
    (unless (= exit-code 0)
      (error "Error calling %s, exit code is %s" command exit-code))))

(defun tree-sitter-langs--update-repos ()
  "Update lang repos' remotes."
  (seq-map
   (lambda (desc)
     (pcase-let* ((`(,lang-symbol . _) desc)
                  (default-directory (concat tree-sitter-langs--repos-dir
                                             (format "tree-sitter-%s" lang-symbol))))
       (tree-sitter-langs--call "git" "remote" "update")))
   tree-sitter-langs-repos))

(defun tree-sitter-langs--get-latest (type)
  "Return the `tree-sitter-langs-repos' with versions replaced by latest tags/commits.
TYPE should be either `:commits' or `:tags'. If there's no tag, return the
latest commit."
  (require 'magit)
  (seq-map
   (lambda (desc)
     (pcase-let* ((`(,lang-symbol _ . ,extra) desc)
                  (default-directory (concat tree-sitter-langs--repos-dir
                                             (format "tree-sitter-%s" lang-symbol))))
       `(,lang-symbol ,(pcase type
                         (:commits (magit-rev-parse "--short=7" "origin/master"))
                         (:tags (or (magit-get-current-tag "origin/master")
                                    (magit-rev-parse "--short=7" "origin/master"))))
                      . ,extra)))
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

(defun tree-sitter-langs-compile (lang-symbol &optional clean)
  "Download and compile the grammar for LANG-SYMBOL.
This function requires git and tree-sitter CLI.

If the optional arg CLEAN is non nil, compile from the revision specified in
`tree-sitter-langs-repos', and clean up afterwards. Otherwise, compile from the
current state of the grammar repo, without cleanup."
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
      (progn
        (tree-sitter-langs--call "git" "clone" "-q" repo dir)
        (let ((default-directory dir))
          ;; Use the specified version on first build. This makes CI runs more
          ;; reproducible.
          (tree-sitter-langs--call "git" "checkout" version))))
    (let ((default-directory dir))
      (when clean
        (tree-sitter-langs--call "git" "stash" "push")
        (tree-sitter-langs--call "git" "checkout" version))
      (tree-sitter-langs--call "npm" "set" "progress=false")
      ;; TODO: Figure out why we need to skip `npm install' for some repos.
      (ignore-errors
        (tree-sitter-langs--call "npm" "install"))
      ;; A repo can have multiple grammars (e.g. typescript + tsx).
      (dolist (path paths)
        (let ((default-directory (file-name-as-directory (concat dir path))))
          (tree-sitter-langs--call "tree-sitter" "generate")
          (tree-sitter-langs--call "tree-sitter" "test")))
      ;; Replace underscores with hyphens. Example: c_sharp.
      (let ((default-directory tree-sitter-langs--bin-dir))
        (dolist (file (directory-files default-directory))
          (when (string-match "_" file)
            (let ((new-name (replace-regexp-in-string "_" "-" file)))
              (when (file-exists-p new-name)
                (delete-file new-name))
              (rename-file file new-name)))))
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
      (when clean
        (tree-sitter-langs--call "git" "reset" "--hard" "HEAD")
        (tree-sitter-langs--call "git" "clean" "-f")))))

(defun tree-sitter-langs-create-bundle (&optional clean)
  "Create a bundle of language grammars.
The bundle includes all languages declared in `tree-sitter-langs-repos'.

If the optional arg CLEAN is non nil, compile from the revisions specified in
`tree-sitter-langs-repos', and clean up afterwards. Otherwise, compile from the
current state of the grammar repos, without cleanup."
  (unless (executable-find "tar")
    (error "Could not find tar executable (needed to bundle compiled grammars)"))
  (let ((errors (thread-last tree-sitter-langs-repos
                  (seq-map
                   (lambda (desc)
                     (pcase-let ((`(,lang-symbol . _) desc))
                       (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                       (condition-case err
                           (tree-sitter-langs-compile lang-symbol clean)
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
          (apply #'tree-sitter-langs--call "tar" "-zcvf" tar-file (append tar-opts files))
          (with-temp-file tree-sitter-langs--bundle-version-file
            (let ((coding-system-for-write 'utf-8))
              (insert tree-sitter-langs--bundle-version))))
      (when errors
        (message "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
        (display-warning 'tree-sitter
                         (format "Could not compile grammars:\n%s" (pp-to-string errors)))))))

;;;###autoload
(defun tree-sitter-langs-install-grammars (&optional skip-if-installed version os keep-bundle)
  "Download and install the specified VERSION of the language grammar bundle.
If VERSION or OS is not specified, use the default of
`tree-sitter-langs--bundle-version' and `tree-sitter-langs--os'.

This installs the grammar bundle even if the same version was already installed,
unless SKIP-IF-INSTALLED is non-nil.

The download bundle file is deleted after installation, unless KEEP-BUNDLE is
non-nil."
  (interactive (list
                nil
                (read-string "Bundle version: " tree-sitter-langs--bundle-version)
                tree-sitter-langs--os
                nil))
  (unless (file-directory-p tree-sitter-langs--bin-dir)
    (make-directory tree-sitter-langs--bin-dir t))
  (let* ((version (or version tree-sitter-langs--bundle-version))
         (default-directory tree-sitter-langs--bin-dir)
         (bundle-file (tree-sitter-langs--bundle-file ".gz" version os))
         (current-version (when (file-exists-p
                                 tree-sitter-langs--bundle-version-file)
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents
                                 tree-sitter-langs--bundle-version-file)
                                (buffer-string))))))
    (cl-block nil
      (if (string= version current-version)
          (if skip-if-installed
              (progn (message "tree-sitter-langs: Grammar bundle v%s was already installed; skipped" version)
                     (cl-return))
            (message "tree-sitter-langs: Grammar bundle v%s was already installed; reinstalling" version))
        (message "tree-sitter-langs: Installing grammar bundle v%s (was v%s)" version current-version))
      ;; FIX: Handle HTTP errors properly.
      (url-copy-file (tree-sitter-langs--bundle-url version os)
                     bundle-file 'ok-if-already-exists)
      (tree-sitter-langs--call "tar" "-xvzf" bundle-file)
      ;; FIX: This should be a metadata file in the bundle itself.
      (with-temp-file tree-sitter-langs--bundle-version-file
        (let ((coding-system-for-write 'utf-8))
          (insert version)))
      (unless keep-bundle
        (delete-file bundle-file 'trash))
      (when (and (called-interactively-p 'any)
                 (y-or-n-p (format "Show installed grammars in %s? " tree-sitter-langs--bin-dir)))
        (with-current-buffer (find-file tree-sitter-langs--bin-dir)
          (when (bound-and-true-p dired-omit-mode)
            (dired-omit-mode -1)))))))

(defun tree-sitter-langs--copy-query (lang-symbol &optional force)
  "Copy highlights.scm file of LANG-SYMBOL to `tree-sitter-langs--queries-dir'.
This assumes the repo has already been set up, for example by
`tree-sitter-langs-compile'."
  (let ((src (thread-first tree-sitter-langs--repos-dir
               (concat (format "tree-sitter-%s" lang-symbol))
               file-name-as-directory (concat "queries")
               file-name-as-directory (concat "highlights.scm"))))
    (when (file-exists-p src)
      (let ((dst-dir  (file-name-as-directory
                       (concat tree-sitter-langs--queries-dir
                               (symbol-name lang-symbol)))))
        (unless (file-directory-p dst-dir)
          (make-directory dst-dir t))
        (message "Copying highlights.scm for %s" lang-symbol)
        (let ((default-directory dst-dir))
          (if (file-exists-p "highlights.scm")
              (when force
                (copy-file src dst-dir :force))
            (copy-file src dst-dir)))))))

(defun tree-sitter-langs--copy-queries ()
  "Copy highlights.scm files to `tree-sitter-langs--queries-dir'.
This assumes the repos have already been cloned set up, for example by
`tree-sitter-langs-create-bundle'."
  (pcase-dolist (`(,lang-symbol . _) tree-sitter-langs-repos)
    (tree-sitter-langs--copy-query lang-symbol :force)))

(provide 'tree-sitter-langs-build)
;;; tree-sitter-langs-build.el ends here
