$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tsc_dyn"
$module_renamed = $module_name.replace("_", "-")
$core_root = "$project_root\core"

Push-Location $project_root
$target = $args[0]
if ($target -eq "release") {
    cargo build --all --release
} else {
    $target = "debug"
    cargo build --all
}
$module_dir = "$project_root\target\$target"

Push-Location $core_root
Copy-Item $module_dir\$module_name.dll $core_root\$module_renamed.dll
$version = ((cargo pkgid) | Out-String).Trim().Split('#')[-1].Split(':')[-1]
Set-Content -Path "DYN-VERSION" -Value "${version}.1" -NoNewLine -Force
cask build
Pop-Location

cask build
Pop-Location
