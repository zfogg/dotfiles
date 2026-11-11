$profileScript = Join-Path (Split-Path -Parent $PROFILE) 'PowerShell\profile.ps1'
if (Test-Path -LiteralPath $profileScript) {
    . $profileScript
}
