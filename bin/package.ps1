$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root

.\bin\build.ps1 release

cask build --trace
cask package --trace

Pop-Location
