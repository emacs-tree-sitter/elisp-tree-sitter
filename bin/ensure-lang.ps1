$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$lang = $args[0]

Push-Location -Path $project_root

emacs --batch `
  --directory "$project_root\lisp" `
  --directory "$project_root\langs" `
  --eval "(progn (require 'tree-sitter-langs) (tree-sitter-langs-ensure '$lang))"

Pop-Location
