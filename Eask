(package "elisp-tree-sitter"
         "0.18.0"
         "Tree-sitter bindings for Emacs Lisp")

(website-url "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
(keywords "tree-sitter")

(package-file "lisp/tree-sitter.el")
(files
 "lisp/*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'melpa)

(depends-on "emacs" "27.1")

(development
 (depends-on "rust-mode")
 (depends-on "async")
 )

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

;; Use GNU tar in Windows
(when (memq system-type '(cygwin windows-nt ms-dos))
  (setq package-build-tar-executable "C:/Program Files/Git/usr/bin/tar.exe"))
