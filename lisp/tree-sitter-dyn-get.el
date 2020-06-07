;;; tree-sitter-dyn-get.el --- Utilities to obtain tree-sitter-dyn -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the utilities to download pre-built binaries for the
;; dynamic module `tree-sitter-dyn'.

;;; Code:

(require 'dired-aux)

(defun tree-sitter-dyn-get--download ()
  "Download the pre-compiled `tree-sitter-dyn' module."
  (let* ((main-file (locate-library "tree-sitter.el"))
         (_ (unless main-file
              (error "Could not find tree-sitter.el")))
         ;; TODO: Version `tree-sitter-dyn' separately from `tree-sitter'.
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
         (uncompressed? (version< "0.7.0" version))
         (url (format "https://github.com/ubolonton/emacs-tree-sitter/releases/download/%s/%s"
                      version (if uncompressed? dyn-file gz-file)))
         (default-directory (file-name-directory main-file)))
    (message "Downloading %s" url)
    (if uncompressed?
        (url-copy-file url dyn-file :ok-if-already-exists)
      (url-copy-file url gz-file)
      (when (file-exists-p dyn-file)
        (delete-file dyn-file))
      ;; XXX: Uncompressing with `dired-compress-file' doesn't work on Windows.
      (dired-compress-file gz-file))))

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
