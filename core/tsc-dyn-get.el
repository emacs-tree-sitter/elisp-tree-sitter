;;; tsc-dyn-get.el --- Utilities to obtain tsc-dyn -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This file contains the utilities to download pre-built binaries for the
;; dynamic module `tsc-dyn'.

;;; Code:

(require 'seq)
(require 'dired-aux)

(eval-when-compile
  ;; Version string set by `tsc-dyn' when it's loaded.
  (defvar tsc-dyn--version))

(defconst tsc-dyn-get--version-file "DYN-VERSION")

(defun tsc-dyn-get--dir ()
  "Return the directory to put `tsc-dyn' module in."
  (let* ((main-file (locate-library "tsc.el"))
         (_ (unless main-file
              (error "Could not find tsc.el"))))
    (file-name-directory main-file)))

(defun tsc-dyn-get--download (version)
  "Download the pre-compiled VERSION of `tsc-dyn' module."
  (let* ((default-directory (tsc-dyn-get--dir))
         ;; TODO: Handle systems with no pre-built binaries better.
         (ext (pcase system-type
                ('windows-nt "dll")
                ('darwin "dylib")
                ('gnu/linux "so")
                (_ (error "Unsupported system-type %s" system-type))))
         (dyn-file (format "tsc-dyn.%s" ext))
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
    (with-temp-file tsc-dyn-get--version-file
      (let ((coding-system-for-write 'utf-8))
        (insert version)))))

(defun tsc--try-load-dyn (file)
  "Try loading `tsc-dyn' from FILE. Return nil if the file does not exist."
  (or (featurep 'tsc-dyn)
      (condition-case _
          (module-load file)
        (module-open-failed nil))))

;; XXX: Using `require' after setting`module-file-suffix' to `.dylib' results in
;; "Cannot open load file: No such file or directory, tsc-dyn".
;;
;; XXX: Using `load' results in an error message with garbled text: "Symbol’s
;; value as variable is void: Ïúíþ".
;;
;; Therefore, we need to search for the file and use `module-load' directly.
(defun tsc--mac-load-dyn ()
  "Search and load the dynamic module on macOS."
  (let ((file "tsc-dyn.dylib"))
    ;; Try directory containing `load-file-name'. Typical case.
    (when load-file-name
      (tsc--try-load-dyn (concat (file-name-directory load-file-name)
                                 file)))
    ;; Try working directory (e.g. when invoked by `cask').
    (tsc--try-load-dyn file)
    ;; Fall back to `load-path'.
    (seq-find (lambda (dir)
                (let ((full-name (concat (file-name-as-directory
                                          (expand-file-name dir))
                                         file)))
                  (tsc--try-load-dyn full-name)))
              load-path)))

(defun tsc-dyn-get-ensure (version)
  "Try to load a specific VERSION of  `tsc-dyn'.
If it's not found, try to download it."
  ;; On Windows, we cannot overwrite the old dll file while it's opened
  ;; (loaded), so we'll have to do the version check before loading the module,
  ;; through a version file.
  (let* ((default-directory (tsc-dyn-get--dir))
         (current-version (when (file-exists-p
                                 tsc-dyn-get--version-file)
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents
                                 tsc-dyn-get--version-file)
                                (buffer-string))))))
    ;; This is also run on CI, after we've built the binaries, but before
    ;; publishing them. Downloading at that time doesn't make sense, so we
    ;; disable it with a special version string.
    (unless (string= current-version "LOCAL")
      (when (or (not current-version)
                (version< current-version version))
        (tsc-dyn-get--download version))))
  ;; XXX: We want a universal package containing binaries for all platforms, so
  ;; we use a unique extension for each. On macOS, we use`.dylib', which is more
  ;; sensible than `.so' anyway.
  (unless (featurep 'tsc-dyn)
    (when (eq system-type 'darwin)
      (tsc--mac-load-dyn)))
  ;; If we could not load it (e.g. when the dynamic module was deleted, but the
  ;; version file was not), try downloading again.
  (unless (require 'tsc-dyn nil :noerror)
    (tsc-dyn-get--download version))
  ;; We should have the binary by now. Try to load for real.
  (unless (featurep 'tsc-dyn)
    (when (eq system-type 'darwin)
      (tsc--mac-load-dyn))
    (require 'tsc-dyn))
  ;; Check if and older version was already loaded.
  (unless (string= version tsc-dyn--version)
    (display-warning 'tree-sitter
                     (format "Version %s of tsc-dyn was already loaded. Please restart Emacs to load the requested version %s"
                             tsc-dyn--version version)
                     :emergency)))

(provide 'tsc-dyn-get)
;;; tsc-dyn-get.el ends here
