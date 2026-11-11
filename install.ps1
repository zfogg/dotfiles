[CmdletBinding()]
param()

$ErrorActionPreference = 'Stop'
$DotfilesDir = $PSScriptRoot
$HomeDir = [Environment]::GetFolderPath('UserProfile')
$BinDir = Join-Path $HomeDir 'bin'
$BackupRoot = Join-Path $HomeDir ('.dotfiles-backup-' + (Get-Date -Format 'yyyyMMdd-HHmmss'))

function Write-Info([string]$Message) { Write-Host "==> $Message" -ForegroundColor Cyan }
function Write-Ok([string]$Message) { Write-Host "  OK $Message" -ForegroundColor Green }
function Write-Warn([string]$Message) { Write-Host "  !  $Message" -ForegroundColor Yellow }

function Test-Command([string]$Name) { return [bool](Get-Command $Name -ErrorAction SilentlyContinue) }

function Ensure-Scoop {
    if (Test-Command scoop) { return }

    Write-Info 'Installing Scoop'
    Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    Invoke-RestMethod -Uri 'https://get.scoop.sh' | Invoke-Expression
    if (-not (Test-Command scoop)) { throw 'Scoop installation completed but scoop is not on PATH. Restart PowerShell and rerun.' }
}

function Ensure-Bucket([string]$Name, [string]$Source) {
    $bucketPath = Join-Path $HomeDir "scoop\buckets\$Name"
    if (-not (Test-Path -LiteralPath $bucketPath -PathType Container)) {
        Write-Info "Adding Scoop bucket $Name"
        if ($Source) { scoop bucket add $Name $Source | Out-Host }
        else { scoop bucket add $Name | Out-Host }
    }
}

function Ensure-App([string]$Name) {
    $appPath = Join-Path $HomeDir "scoop\apps\$Name\current"
    if (Test-Path -LiteralPath $appPath -PathType Container) {
        Write-Ok "$Name already installed"
        return
    }
    Write-Info "Installing $Name"
    scoop install $Name | Out-Host
}

function Backup-Item([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path)) { return }
    New-Item -ItemType Directory -Path $BackupRoot -Force | Out-Null
    $name = Split-Path $Path -Leaf
    $destination = Join-Path $BackupRoot $name
    $suffix = 1
    while (Test-Path -LiteralPath $destination) {
        $destination = Join-Path $BackupRoot ("$name.$suffix")
        $suffix++
    }
    Move-Item -LiteralPath $Path -Destination $destination
}

function Ensure-DirectoryLink([string]$Link, [string]$Target) {
    if (Test-Path -LiteralPath $Link -PathType Container) {
        $item = Get-Item -LiteralPath $Link -Force
        if ($item.Attributes -band [IO.FileAttributes]::ReparsePoint -and $item.Target -eq $Target) {
            Write-Ok "$Link already links to $Target"
            return
        }
        if (@(Get-ChildItem -LiteralPath $Link -Force).Count -eq 0) {
            Remove-Item -LiteralPath $Link
        } else {
            Backup-Item $Link
        }
    } elseif (Test-Path -LiteralPath $Link) {
        Backup-Item $Link
    }

    $parent = Split-Path $Link -Parent
    New-Item -ItemType Directory -Path $parent -Force | Out-Null
    cmd.exe /d /c "mklink /D `"$Link`" `"$Target`"" | Out-Host
    if (-not (Test-Path -LiteralPath $Link)) { throw "Failed to create directory link $Link" }
    Write-Ok "$Link -> $Target"
}

function Ensure-FileLink([string]$Link, [string]$Target) {
    if (Test-Path -LiteralPath $Link) {
        Write-Warn "Preserving existing PowerShell profile: $Link"
        return
    }
    New-Item -ItemType Directory -Path (Split-Path $Link -Parent) -Force | Out-Null
    cmd.exe /d /c "mklink `"$Link`" `"$Target`"" | Out-Host
    if (-not (Test-Path -LiteralPath $Link)) { throw "Failed to create file link $Link" }
    Write-Ok "$Link -> $Target"
}

function Ensure-ManagedFileLink([string]$Link, [string]$Target) {
    if (Test-Path -LiteralPath $Link) {
        $item = Get-Item -LiteralPath $Link -Force
        if (($item.Attributes -band [IO.FileAttributes]::ReparsePoint) -and $item.Target -eq $Target) {
            Write-Ok "$Link already links to $Target"
            return
        }
        Backup-Item $Link
    }
    New-Item -ItemType Directory -Path (Split-Path $Link -Parent) -Force | Out-Null
    cmd.exe /d /c "mklink `"$Link`" `"$Target`"" | Out-Host
    if (-not (Test-Path -LiteralPath $Link)) { throw "Failed to create file link $Link" }
    Write-Ok "$Link -> $Target"
}

function Ensure-UserPathEntry([string]$Entry) {
    $userPath = [Environment]::GetEnvironmentVariable('Path', 'User')
    $entries = @($userPath -split ';' | Where-Object { $_ })
    if (-not ($entries | Where-Object { $_.TrimEnd('\') -ieq $Entry.TrimEnd('\') })) {
        [Environment]::SetEnvironmentVariable('Path', (($entries + $Entry) -join ';'), 'User')
        $env:Path = "$Entry;$env:Path"
        Write-Ok "Added $Entry to user PATH"
    } else {
        Write-Ok "$Entry already on user PATH"
    }
}

function Ensure-NeovimPlugins {
    if (-not (Test-Command nvim)) {
        Write-Warn 'Neovim not found; skipping plugin installation'
        return
    }

    $lazyRoot = if ($env:XDG_DATA_HOME) {
        Join-Path $env:XDG_DATA_HOME 'nvim'
    } else {
        Join-Path $env:LOCALAPPDATA 'nvim-data'
    }
    $lazyPath = Join-Path $lazyRoot 'lazy\lazy.nvim'
    if (Test-Path -LiteralPath $lazyPath -PathType Container) {
        Write-Ok 'Neovim dependencies already cloned'
        return
    }

    Write-Info 'Installing Neovim dependencies with lazy.nvim'
    nvim --headless '+Lazy! sync' +qa | Out-Host
    Write-Ok 'Neovim dependencies installed'
}

Write-Info "Installing Windows dotfiles from $DotfilesDir"

Ensure-Scoop
Ensure-Bucket 'main' $null
Ensure-Bucket 'extras' $null
Ensure-Bucket 'supabase' 'https://github.com/supabase/scoop-bucket.git'

# Install/update the PowerShell 7 host only when it is absent. The installer
# runs from either Windows PowerShell or PowerShell 7, so use pwsh.exe as the
# presence check rather than relying on the current host.
if (-not (Test-Command pwsh)) {
    Ensure-App 'pwsh'
}

$Packages = @(
    '1password-cli', '7zip', '7zip19.00-helper', 'aws', 'base64', 'bat', 'binaryen', 'btop',
    'bun', 'cacert', 'caddy', 'ccache', 'cmake', 'dark', 'delta', 'deno', 'direnv', 'doxygen',
    'drmemory', 'dust', 'edit', 'emscripten', 'eza', 'fd', 'ffmpeg', 'fzf', 'gcc', 'gdb', 'gh',
    'go', 'graphviz', 'gzip', 'imagemagick', 'innounp', 'jq', 'lazygit', 'llvm', 'lua', 'make',
    'mise', 'msys2', 'neovim', 'ninja', 'nodejs', 'ntop', 'opencode', 'perl', 'pkg-config',
    'pnpm', 'postgresql', 'python', 'redis', 'ripgrep', 'rustup', 'scoop-search', 'sdl2', 'slack',
    'supabase', 'swig', 'tar', 'tealdeer', 'unxutils', 'uv', 'vcpkg', 'vcredist2022', 'vscode',
    'wget', 'windirstat', 'xz', 'zip'
)
foreach ($package in $Packages) { Ensure-App $package }
Ensure-NeovimPlugins

$RepoBin = Join-Path $DotfilesDir 'bin'
New-Item -ItemType Directory -Path $RepoBin -Force | Out-Null
if ((Test-Path -LiteralPath $BinDir) -and -not ((Get-Item -LiteralPath $BinDir -Force).Attributes -band [IO.FileAttributes]::ReparsePoint)) {
    Write-Info "Merging existing $BinDir into $RepoBin"
    foreach ($item in Get-ChildItem -LiteralPath $BinDir -Force) {
        $destination = Join-Path $RepoBin $item.Name
        if (Test-Path -LiteralPath $destination) {
            if ((Get-FileHash -LiteralPath $item.FullName -Algorithm SHA256 -ErrorAction SilentlyContinue).Hash -eq (Get-FileHash -LiteralPath $destination -Algorithm SHA256 -ErrorAction SilentlyContinue).Hash) {
                Remove-Item -LiteralPath $item.FullName -Force
            } else {
                Write-Warn "Keeping existing repo item; backing up $($item.Name)"
                Backup-Item $item.FullName
            }
        } else {
            Move-Item -LiteralPath $item.FullName -Destination $destination
        }
    }
}
Ensure-DirectoryLink $BinDir $RepoBin
Ensure-UserPathEntry $BinDir
Ensure-UserPathEntry (Join-Path $HomeDir 'scoop\shims')

# Keep both PowerShell hosts on the checked-in profile. The first path is the
# current-user profile used by PowerShell 7; the second is Windows PowerShell 5.
$PwshProfile = Join-Path ([Environment]::GetFolderPath('MyDocuments')) 'PowerShell\Microsoft.PowerShell_profile.ps1'
$WindowsPowerShellProfile = Join-Path ([Environment]::GetFolderPath('MyDocuments')) 'WindowsPowerShell\Microsoft.PowerShell_profile.ps1'
Ensure-FileLink $PwshProfile (Join-Path $DotfilesDir 'Documents\PowerShell\Microsoft.PowerShell_profile.ps1')
Ensure-FileLink $WindowsPowerShellProfile (Join-Path $DotfilesDir 'Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1')

$HyperConfig = Join-Path $HomeDir 'AppData\Roaming\Hyper\.hyper.js'
Ensure-ManagedFileLink $HyperConfig (Join-Path $DotfilesDir '.hyper.js')

Write-Ok 'Windows dotfiles installation complete'
if (Test-Path -LiteralPath $BackupRoot) { Write-Warn "Backups are in $BackupRoot" }
Write-Host 'Restart PowerShell to refresh PATH and profiles.'
