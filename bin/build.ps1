$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tree_sitter_dyn"
$module_renamed = $module_name.replace("_", "-")

$target = $args[0]
if ($target -eq "release") {
    $extra = "--release"
} else {
    $target = "debug"
    $extra = ""
}

$module_dir = "$project_root\target\$target"

Push-Location $project_root

cargo build --all $extra

Pop-Location

Copy-Item $module_dir\$module_name.dll $project_root\lisp\$module_renamed.dll
