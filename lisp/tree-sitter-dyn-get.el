;;; tree-sitter-dyn-get.el --- Utilities to obtain tree-sitter-dyn -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the utilities to download pre-built binaries for the
;; dynamic module `tree-sitter-dyn'.

;;; Code:

(require 'dired-aux)
(require 'tar-mode)

(defconst tree-sitter-dyn-get--version "0.7.0"
  "The expected version of `tree-sitter-dyn'.")

(defun tree-sitter-dyn-get--download ()
  "Download the pre-compiled `tree-sitter-dyn' module."
  (let* ((main-file (locate-library "tree-sitter.el"))
         (_ (unless main-file
              (error "Could not find tree-sitter.el")))
         (ext (pcase system-type
                ('windows-nt "dll")
                ('darwin "dylib")
                ('gnu/linux "so")
                (_ (error "Unsupported system-type %s" system-type))))
         (dyn-file (format "tree-sitter-dyn.%s" ext))
         ;; FIX: We have to download the whole tar archive, instead of one
         ;; single binary, because uncompressing with `dired-compress-file'
         ;; doesn't work on Windows.
         (archive-file (format "tree-sitter-%s.tar.gz" tree-sitter-dyn-get--version))
         (url (format "https://github.com/ubolonton/emacs-tree-sitter/releases/download/%s/%s"
                      tree-sitter-dyn-get--version archive-file))
         (main-dir (file-name-directory main-file))
         (default-directory main-dir)
         (archive-dir (file-name-as-directory
                       (expand-file-name
                        (format "tree-sitter-%s" tree-sitter-dyn-get--version)))))
    (message "Downloading %s" url)
    (unless (file-exists-p archive-file)
      (url-copy-file url archive-file))
    (with-temp-buffer
      (insert-file-contents archive-file)
      (tar-mode)
      (tar-untar-buffer))
    (when (file-exists-p dyn-file)
      (delete-file dyn-file))
    (let ((default-directory archive-dir))
      (copy-file dyn-file main-dir))
    (delete-directory archive-dir :recursive)
    (delete-file archive-file)))

(defun tree-sitter-dyn-get-ensure ()
  ;; XXX: We want a universal package containing binaries for all platforms, so
  ;; we use a unique extension for each. On macOS, we use`.dylib', which is more
  ;; sensible than `.so' anyway.
  (unless (featurep 'tree-sitter-dyn)
    (when (eq system-type 'darwin)
      (load "tree-sitter--mac-load.el")))

  ;; TODO: If the currently loaded version of `tree-sitter-dyn' is too old,
  ;; delete it, then restart Emacs (or ask user to do so).

  ;; If we could not load it, try downloading.
  (unless (require 'tree-sitter-dyn nil :noerror)
    ;; TODO: Handle systems with no pre-built binaries.
    (tree-sitter-dyn-get--download))

  ;; We should have the binary by now. Try to load for real.
  (unless (featurep 'tree-sitter-dyn)
    (when (eq system-type 'darwin)
      (load "tree-sitter--mac-load.el"))
    (require 'tree-sitter-dyn)))

(provide 'tree-sitter-dyn-get)
