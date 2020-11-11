(require 'tree-sitter)
(require 'tree-sitter-hl)

;;;###autoload
(define-derived-mode wat-mode prog-mode "WAT"
  ;; It's up to the major mode to set this. It plays a role similar to that of
  ;; `font-lock-defaults'.
  (setq tree-sitter-hl-default-patterns
        [["module" "func" "param"] @keyword
         (module_field_func (identifier) @function)])
  (tree-sitter-hl-mode))

;;;###autoload
(progn
  ;; There are 3 ways to register/load the compiled grammar.
  ;;
  ;; 1. `wat-mode' distributes the compiled grammar files on its own:
  ;;
  ;; (tree-sitter-load 'wat PATH-TO-COMPILED-WAT-GRAMMAR)
  ;;
  ;; 2. Same as above, but with lazy loading:
  ;;
  ;; (add-to-list 'tree-sitter-load-path PATH-TO-DIR-OF-COMPILED-WAT-GRAMMAR)
  ;;
  ;; 3. `tree-sitter-langs' distributes the compiled grammar (not the queries):
  ;; Do nothing.

  ;; Register the association with `tree-sitter-mode'.
  (add-to-list 'tree-sitter-major-mode-language-alist '(wat-mode . wat))
  (add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-mode)))

(provide 'wat-mode)
;;; wat-mode.el ends here
