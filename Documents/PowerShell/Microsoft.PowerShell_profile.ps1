If ($host.Name -eq 'ConsoleHost') {Import-Module PSReadline}


#try { $null = gcm pshazz -ea stop; pshazz init } catch { }

#$DefaultUser = 'zachf'
Import-Module posh-git
Import-Module oh-my-posh
#Set-Theme Paradox
#oh-my-posh --init --shell pwsh --config ~/.omp.pwsh.yaml | Invoke-Expression


# Chocolatey profile
$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}


# Aliases and shortcuts
Set-Alias pbcopy  Set-Clipboard
Set-Alias pbpaste Get-Clipboard

Set-Alias which Get-Command

Set-Alias v     nvim
Set-Alias vim   nvim


If (-Not (Test-Path Variable:PSise)) {
  # Only run this in the console and not in the ISE
  Import-Module Get-ChildItemColor
  Set-Alias l Get-ChildItem -option AllScope
  Set-Alias ls Get-ChildItemColorFormatWide -option AllScope
}

$(/usr/local/bin/brew shellenv) | Invoke-Expression
