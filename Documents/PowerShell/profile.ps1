Import-Module "$HOME\scoop\apps\vcpkg\current\scripts\posh-vcpkg"


# Enable UTF-8 mode:
# INFO: https://stackoverflow.com/q/51933189/672346
$OutputEncoding = [System.Console]::OutputEncoding = [System.Console]::InputEncoding = [System.Text.Encoding]::UTF8
$PSDefaultParameterValues['*:Encoding'] = 'utf8'


$env:HOME = $HOME
$env:DIRENV_LOG_FORMAT = ""


$completionsPath = "$HOME\Documents\PowerShell\Completions"
if (Test-Path $completionsPath) {
    Get-ChildItem -Path $completionsPath -Filter "*.ps1" | ForEach-Object {
        . $_.FullName
    }
}


# Remove SHELL before loading direnv hook to prevent bash invocation
Remove-Item Env:\SHELL -ErrorAction SilentlyContinue


Invoke-Expression (direnv hook pwsh | Out-String)


# https://mise.jdx.dev/installing-mise.html
(&mise activate pwsh) | Out-String | Invoke-Expression


# Aliases and shortcuts
Set-Alias pbcopy  Set-Clipboard
Set-Alias pbpaste Get-Clipboard

Set-Alias -Name vim -Value nvim
Set-Alias -Name vi -Value nvim
Set-Alias -Name v -Value nvim

function edit-profile { nvim $PROFILE.CurrentUserAllHosts }
function reload-profile {
  Import-Module $PROFILE.AllUsersAllHosts        2>$null
  Import-Module $PROFILE.AllUsersCurrentHost     2>$null
  Import-Module $PROFILE.CurrentUserAllHosts     2>$null
  Import-Module $PROFILE.CurrentUserCurrentHost  2>$null
  Import-Module $PROFILE                         2>$null
  echo "👤🔄️ Profile reloaded."
}

function gst { git status }
Remove-Item Alias:\gp -Force -ErrorAction SilentlyContinue
function gp  { git push @args }
function gca { git commit -a @args }
function gd  { git diff @args }

function ls { eza }
function la { eza -la }

function Duration-Of-Last-Command {
    $command = Get-History -Count 1
    $duration = $command.EndExecutionTime - $command.StartExecutionTime
    $formatted = if ($duration.TotalHours -ge 1) {
        "{0}:{1:mm\:ss\.fff}" -f [int]$duration.TotalHours, $duration
    } elseif ($duration.TotalMinutes -ge 1) {
        $duration.ToString("m\:ss\.fff")
    } else {
        $duration.ToString("s\.fff")
    }
    $color = if ($duration.TotalSeconds -ge 50) { "`e[31m" }
              elseif ($duration.TotalSeconds -ge 15) { "`e[33m" }
              else { "`e[32m" }
    "$color$formatted`e[0m"
}

function l {
    param(
        [Parameter(ValueFromRemainingArguments=$true)]
        [string[]]$Args
    )

    Get-ChildItem @Args | ForEach-Object {
        $size = if ($_.PSIsContainer) { '' }
            elseif ($_.Length -ge 1GB) { '{0:N2} GB' -f ($_.Length / 1GB) }
            elseif ($_.Length -ge 1MB) { '{0:N2} MB' -f ($_.Length / 1MB) }
            elseif ($_.Length -ge 1KB) { '{0:N2} KB' -f ($_.Length / 1KB) }
            else { '{0} B' -f $_.Length }

        $isSymlink = $_.Attributes -band [System.IO.FileAttributes]::ReparsePoint

        $color = if ($isSymlink) { 'Cyan' }
            elseif ($_.PSIsContainer) { 'Blue' }
            elseif ($_.Name -match '\.(exe|cmd|bat|ps1|sh|bash)$') { 'Yellow' }
            else { 'White' }

        $sizeColor = if ($_.Length -ge 1GB) { 'Red' }
            elseif ($_.Length -ge 100MB) { 'Yellow' }
            else { 'Gray' }

        Write-Host ("{0,-12}" -f $_.Mode) -NoNewline
        Write-Host ("{0,20}" -f $_.LastWriteTime.ToString("g")) -NoNewline -ForegroundColor DarkGray
        Write-Host ("{0,12}" -f $size) -NoNewline -ForegroundColor $sizeColor

        if ($isSymlink) {
            $target = $_.LinkTarget
            if (-not $target -and $_.Target) {
                $target = $_.Target
            }
            if (-not $target) {
                $fsout = fsutil reparsepoint query $_.FullName 2>$null
                if ($fsout) {
                    $hexBytes = @()
                    $fsout | Select-String '^\d{4}:' | ForEach-Object {
                        $parts = $_.Line -split '\s{2,}'
                        if ($parts.Count -ge 2) {
                            $hexPart = $parts[1..($parts.Count-2)] -join ' '
                            $hexBytes += ($hexPart -split '\s+' | Where-Object { $_ -match '^[0-9a-f]{2}$' })
                        }
                    }
                    if ($hexBytes.Count -gt 4) {
                        $bytes = [byte[]]($hexBytes | ForEach-Object { [convert]::ToByte($_, 16) })
                        $target = [System.Text.Encoding]::UTF8.GetString($bytes[4..($bytes.Count-1)])
                    }
                }
            }
            Write-Host "  $($_.Name)" -NoNewline -ForegroundColor Cyan
            if ($target) {
                Write-Host ' -> ' -NoNewline -ForegroundColor DarkGray
                Write-Host "$target" -ForegroundColor Cyan
            }
            else {
                Write-Host ''
            }
        } else {
            Write-Host "  $($_.Name)" -ForegroundColor $color
        }
    }
}

Set-Alias -Name la -Value l

function bat { & bat.exe --paging=never @args }
function c   { & bat.exe --paging=never @args }

function Get-Size {
    # INFO: https://superuser.com/a/631094/132491
    param([string]$pth)
    "{0:n2}" -f ((gci -path $pth -recurse | measure-object -property length -sum).sum /1mb) + " mb"
}

