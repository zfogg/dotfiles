# Vite+ environment
. "$env:APPDATA\vite-plus\env.ps1"

# Expose the Command Prompt mklink built-in in PowerShell.
function mklink { & $env:ComSpec /c mklink @args }

# Use the Windows OpenSSH client, which connects to the ssh-agent service.
function ssh-add { & 'C:\Windows\System32\OpenSSH\ssh-add.exe' @args }
