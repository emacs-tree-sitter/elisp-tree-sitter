$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$core_root = "$project_root\core"

Push-Location $core_root

cask build
cask package

$version = ((cask version) | Out-String).Trim()
$tar_file = "dist\lts-$version.tar"
gzip --verbose $tar_file --stdout > "$tar_file.gz"
tar --gzip --list --file "$tar_file"

Pop-Location

Push-Location $project_root

cask build
cask package

$version = ((cask version) | Out-String).Trim()
$tar_file = "dist\tree-sitter-$version.tar"
gzip --verbose $tar_file --stdout > "$tar_file.gz"
tar --gzip --list --file "$tar_file"

Pop-Location
