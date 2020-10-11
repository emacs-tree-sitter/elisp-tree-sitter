$here = $PSScriptRoot
$project_root = (Get-Item $here).Parent.FullName
$core_root = "$project_root\core"

if ($args[0] -eq "watch") {
    Push-Location $core_root
    cargo watch -s "powershell ..\bin\build.ps1" -s "powershell ..\bin\test.ps1"
    Pop-Location
} else {
    # XXX: It seems that Emacs writes to stderr, so PowerShell thinks it's an error. Redirecting to
    # stdout alone doesn't help, because it's the processed stderr, which contain error records, not
    # the original stderr. Piping at the end to convert these error records into strings doesn't
    # work either.
    #
    # It's worth noting that the issue happens only on Azure Pipelines, with Windows 2019, probably
    # because of the execution mode being remote or something.
    #
    # https://mnaoumov.wordpress.com/2015/01/11/execution-of-external-commands-in-powershell-done-right/
    # https://github.com/PowerShell/JEA/issues/24
    # https://github.com/PowerShell/PowerShell/issues/4002
    # https://stackoverflow.com/questions/2095088/error-when-calling-3rd-party-executable-from-powershell-when-using-an-ide
    $ErrorActionPreference = 'Continue'
    Push-Location $project_root
    cask emacs --batch `
      --directory "$project_root\core" `
      --directory "$project_root\lisp" `
      --directory "$project_root\langs" `
      -l ert `
      -l tree-sitter-tests.el `
      -f ert-run-tests-batch-and-exit
    Pop-Location
}
