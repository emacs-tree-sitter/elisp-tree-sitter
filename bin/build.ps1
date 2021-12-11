param (
    [string]$profile = "release"
)

$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$module_name = "tsc_dyn"
$module_renamed = $module_name.replace("_", "-")
$core_root = "$project_root\core"

echo "!! Building the dynamic module"
Push-Location $core_root
try {
    switch ($profile) {
        'debug' { cargo build --all }
        'release' { cargo build --all --release }
        default { throw "Unknown profile $profile" }
    }

    Copy-Item "target\$profile\${module_name}.dll" "${module_renamed}.dll"
    $version = ((cargo pkgid) | Out-String).Trim().Split('#')[-1].Split(':')[-1]
    Set-Content -Path "DYN-VERSION" -Value "${version}" -NoNewLine -Force
    cask build
} finally {
    Pop-Location
}

echo "!! Building Lisp code"
Push-Location $project_root
try {
    cask build
} finally {
    Pop-Location
}
