;;; lts-dyn-get.el --- Utilities to obtain lts-dyn -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020  Tuấn-Anh Nguyễn
;;
;; Author: Tuấn-Anh Nguyễn <ubolonton@gmail.com>

;;; Commentary:

;; This file contains the utilities to download pre-built binaries for the
;; dynamic module `lts-dyn'.

;;; Code:

(require 'dired-aux)

(defconst lts-dyn-get--version-file "DYN-VERSION")

(defun lts-dyn-get--dir ()
  "Return the directory to put `lts-dyn' module in."
  (let* ((main-file (locate-library "lts.el"))
         (_ (unless main-file
              (error "Could not find lts.el"))))
    (file-name-directory main-file)))

(defun lts-dyn-get--download (version)
  "Download the pre-compiled VERSION of `lts-dyn' module."
  (let* ((default-directory (lts-dyn-get--dir))
         ;; TODO: Handle systems with no pre-built binaries better.
         (ext (pcase system-type
                ('windows-nt "dll")
                ('darwin "dylib")
                ('gnu/linux "so")
                (_ (error "Unsupported system-type %s" system-type))))
         (dyn-file (format "lts-dyn.%s" ext))
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
    (with-temp-file lts-dyn-get--version-file
      (let ((coding-system-for-write 'utf-8))
        (insert version)))))

(defun lts-dyn-get-ensure (version)
  "Try to load a specific VERSION of  `lts-dyn'.
If it's not found, try to download it."
  ;; On Windows, we cannot overwrite the old dll file while it's opened
  ;; (loaded), so we'll have to do the version check before loading the module,
  ;; through a version file.
  (let* ((default-directory (lts-dyn-get--dir))
         (current-version (when (file-exists-p
                                 lts-dyn-get--version-file)
                            (with-temp-buffer
                              (let ((coding-system-for-read 'utf-8))
                                (insert-file-contents
                                 lts-dyn-get--version-file)
                                (buffer-string))))))
    ;; This is also run on CI, after we've built the binaries, but before
    ;; publishing them. Downloading at that time doesn't make sense, so we
    ;; disable it with a special version string.
    (unless (string= current-version "LOCAL")
      (when (or (not current-version)
                (version< current-version version))
        (lts-dyn-get--download version))))

  ;; TODO: If the currently loaded version of `lts-dyn' is too old,
  ;; restart Emacs (or ask user to do so).

  ;; XXX: We want a universal package containing binaries for all platforms, so
  ;; we use a unique extension for each. On macOS, we use`.dylib', which is more
  ;; sensible than `.so' anyway.
  (unless (featurep 'lts-dyn)
    (when (eq system-type 'darwin)
      (load "lts--mac-load.el")))

  ;; If we could not load it (e.g. when the dynamic module was deleted, but the
  ;; version file was not), try downloading again.
  (unless (require 'lts-dyn nil :noerror)
    (lts-dyn-get--download version))

  ;; We should have the binary by now. Try to load for real.
  (unless (featurep 'lts-dyn)
    (when (eq system-type 'darwin)
      (load "lts--mac-load.el"))
    (require 'lts-dyn)))

(provide 'lts-dyn-get)
;;; lts-dyn-get.el ends here
