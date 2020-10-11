(package-file "lisp/tree-sitter.el")

(files "lisp/*.el")

(source melpa)

(development
 (depends-on "rust-mode")
 ;; XXX: This is a hack to work around Cask's lack of support for multiple packages in 1 codebase.
 (depends-on "lts"
             :git ".git" :branch "lts"
             :files ("core/*.el"
                     "core/DYN-VERSION"
                     "core/lts-dyn.dylib"
                     "core/lts-dyn.so"
                     "core/lts-dyn.dll")))
