$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root

# XXX: Create the directory because `cask link` doesn't.
$cask_package_dir = ((cask package-directory) | Out-String).Trim()
New-Item -ItemType Directory -Force -Path "$cask_package_dir"
cask link tsc core
cask install

Pop-Location
