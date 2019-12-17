$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName

Push-Location $project_root

.\bin\build.ps1 release

function cask([String] $command) {
    emacs -Q --script "$env:UserProfile\.cask\cask-cli.el" -- $command --trace
}

cask build
cask package

Pop-Location
