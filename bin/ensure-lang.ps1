$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$lang = $args[0]

New-Item -ItemType Directory -Force -Path "$project_root\grammars"

$lang_dir = "$project_root\grammars\tree-sitter-$lang"


if (!(Test-Path -PathType Container $lang_dir)) {
    git clone "https://github.com/tree-sitter/tree-sitter-$lang" $lang_dir
}


Push-Location -Path $lang_dir

git remote update
git reset --hard origin/HEAD

echo "Running 'tree-sitter test' for $lang"
tree-sitter test

Pop-Location
