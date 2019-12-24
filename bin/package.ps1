$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root

.\bin\build.ps1 release

cask build
cask package

$version = ((cask version) | Out-String).Trim()
$tar_file = "dist\tree-sitter-$version.tar"
gzip --verbose $tar_file --stdout > "$tar_file.gz"

ls dist

Pop-Location
