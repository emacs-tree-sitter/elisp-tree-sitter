$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tree_sitter_dyn"
$module_renamed = $module_name.replace("_", "-")
$core_root = "$project_root\core"

$target = $args[0]
if ($target -eq "release") {
    $extra = "--release"
} else {
    $target = "debug"
    $extra = ""
}

$module_dir = "$core_root\target\$target"

Push-Location $core_root

cargo build --all $extra
Copy-Item $module_dir\$module_name.dll $core_root\$module_renamed.dll

$version = ((cargo pkgid) | Out-String).Trim().Split('#')[-1].Split(':')[-1]
Set-Content -Path "$core_root\DYN-VERSION" -Value $version -NoNewLine -Force

cask build

Pop-Location

Push-Location $project_root

cask build

Pop-Location
