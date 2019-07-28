$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tree_sitter_dyn"
$module_renamed = $module_name.replace("_", "-")
$target = "debug"
$module_dir = "$project_root\target\$target"

if ($args[0] -eq "watch") {
    Push-Location $project_root
    cargo watch --ignore "$module_renamed.dll"  -s "powershell bin\build.ps1" -s "powershell bin\test.ps1"
    Pop-Location
} else {
    emacs --batch `
      --directory $project_root `
      -l ert `
      -l tree-sitter-tests `
      -f ert-run-tests-batch-and-exit
}
