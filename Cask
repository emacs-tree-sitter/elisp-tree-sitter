(package-file "lisp/tree-sitter.el")

(files "lisp/*.el")

(source melpa)

(development
 (depends-on "rust-mode")
 ;; XXX: This is a hack to work around Cask's lack of support for multiple packages in 1 codebase.
 (depends-on "tsc"
             :git ".git" :branch "tsc"
             :files ("core/*.el"
                     "core/DYN-VERSION"
                     "core/tsc-dyn.dylib"
                     "core/tsc-dyn.so"
                     "core/tsc-dyn.dll")))
