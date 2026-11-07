# Vite+ environment
. "$env:APPDATA\vite-plus\env.ps1"

# Expose the Command Prompt mklink built-in in PowerShell.
function mklink { & $env:ComSpec /c mklink @args }
