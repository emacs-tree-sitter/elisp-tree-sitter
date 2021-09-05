$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tsc_dyn"
$module_renamed = $module_name.replace("_", "-")
$core_root = "$project_root\core"

$target = $args[0]

Push-Location $core_root
try {
    if ($target -eq "debug") {
        cargo build --all
    } else {
        $target = "release"
        cargo build --all --release
    }

    Copy-Item "target\$target\${module_name}.dll" "${module_renamed}.dll"
    $version = ((cargo pkgid) | Out-String).Trim().Split('#')[-1].Split(':')[-1]
    Set-Content -Path "DYN-VERSION" -Value "${version}.1" -NoNewLine -Force
    cask build
} finally {
    Pop-Location
}

Push-Location $project_root
try {
    cask build
} finally {
    Pop-Location
}
