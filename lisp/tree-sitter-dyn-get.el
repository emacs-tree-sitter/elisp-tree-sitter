;;; tree-sitter-dyn-get.el --- Utilities to obtain tree-sitter-dyn -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the utilities to download pre-built binaries for the
;; dynamic module `tree-sitter-dyn'.

;;; Code:

(require 'dired-aux)

(defconst tree-sitter-dyn-get--version-file "DYN-VERSION")

(defun tree-sitter-dyn-get--dir ()
  "Return the directory to put `tree-sitter-dyn' module in."
  (let* ((main-file (locate-library "tree-sitter-core"))
         (_ (unless main-file
              (error "Could not find tree-sitter-core"))))
    (file-name-directory main-file)))

(defun tree-sitter-dyn-get--download (version)
  "Download the pre-compiled VERSION of `tree-sitter-dyn' module."
  (let* ((default-directory (tree-sitter-dyn-get--dir))
         ;; TODO: Handle systems with no pre-built binaries better.
         (ext (pcase system-type
                ('windows-nt "dll")
                ('darwin "dylib")
                ('gnu/linux "so")
                (_ (error "Unsupported system-type %s" system-type))))
         (dyn-file (format "tree-sitter-dyn.%s" ext))
         (gz-file (format "%s.gz" dyn-file))
         (uncompressed? (version< "0.7.0" version))
         (url (format "https://github.com/ubolonton/emacs-tree-sitter/releases/download/%s/%s"
                      version (if uncompressed? dyn-file gz-file))))
    (message "Downloading %s" url)
    (if uncompressed?
        (url-copy-file url dyn-file :ok-if-already-exists)
      (url-copy-file url gz-file)
      (when (file-exists-p dyn-file)
        (delete-file dyn-file))
      ;; XXX: Uncompressing with `dired-compress-file' doesn't work on Windows.
      (dired-compress-file gz-file))
    (with-temp-file tree-sitter-dyn-get--version-file
      (let ((coding-system-for-write 'utf-8))
        (insert version)))))

(defun tree-sitter-dyn-get-ensure (version)
  "Try to load a specific VERSION of  `tree-sitter-dyn'.
If it's not found, try to download it."
  ;; On Windows, we cannot overwrite the old dll file while it's opened
  ;; (loaded), so we'll have to do the version check before loading the module,
  ;; through a version file.
  (let* ((default-directory (tree-sitter-dyn-get--dir))
         (current-version (when (file-exists-p
                                 tree-sitter-dyn-get--version-file)
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents
                                 tree-sitter-dyn-get--version-file)
                                (buffer-string))))))
    ;; TODO: Write the correct version as part of the build process.
    (unless (string= current-version "LOCAL")
      (when (or (not current-version)
                (version< current-version version))
        (tree-sitter-dyn-get--download version))))

  ;; TODO: If the currently loaded version of `tree-sitter-dyn' is too old,
  ;; restart Emacs (or ask user to do so).

  ;; XXX: We want a universal package containing binaries for all platforms, so
  ;; we use a unique extension for each. On macOS, we use`.dylib', which is more
  ;; sensible than `.so' anyway.
  (unless (featurep 'tree-sitter-dyn)
    (when (eq system-type 'darwin)
      (load "tree-sitter--mac-load.el")))

  ;; If we could not load it (e.g. when the dynamic module was deleted, but the
  ;; version file was not), try downloading again.
  (unless (require 'tree-sitter-dyn nil :noerror)
    (tree-sitter-dyn-get--download version))

  ;; We should have the binary by now. Try to load for real.
  (unless (featurep 'tree-sitter-dyn)
    (when (eq system-type 'darwin)
      (load "tree-sitter--mac-load.el"))
    (require 'tree-sitter-dyn)))

(provide 'tree-sitter-dyn-get)
;;; tree-sitter-dyn-get.el ends here
