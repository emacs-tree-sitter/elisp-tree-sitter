$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root
try {
    # XXX: Create the directory because `eask link` doesn't.
    #$eask_package_dir = ((eask package-directory) | Out-String).Trim()
    #New-Item -ItemType Directory -Force -Path "$eask_package_dir"
    eask link add tsc core
    eask install
} finally {
    Pop-Location
}
