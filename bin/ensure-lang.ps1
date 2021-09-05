$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$lang = $args[0]

Push-Location -Path $project_root
try {
    emacs --batch `
      --directory "$project_root\core" `
      --directory "$project_root\lisp" `
      --directory "$project_root\langs" `
      --eval "(progn (setq tree-sitter-langs--testing t) (require 'tree-sitter-langs) (tree-sitter-langs-ensure '$lang))"
} finally {
    Pop-Location
}
