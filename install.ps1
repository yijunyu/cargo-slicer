# install.ps1 — Install cargo-slicer from precompiled binaries (Windows)
#
# Usage:
#   irm https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.ps1 | iex
#
# Behind a corporate proxy:
#   $env:HTTPS_PROXY = 'http://proxy.company.com:8080'
#   irm https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.ps1 -Proxy $env:HTTPS_PROXY | iex
#
# What it installs:
#   ~/.cargo/bin/cargo-slicer.exe           — Pre-analysis CLI
#   ~/.cargo/bin/cargo-slicer-rustc.exe     — Rustc driver (MIR analysis + codegen filtering)
#   ~/.cargo/bin/cargo_slicer_dispatch.exe  — RUSTC_WRAPPER dispatcher
#   ~/.cargo/bin/cargo-slicer.ps1           — Drop-in build script

$ErrorActionPreference = 'Stop'

# -- Configure proxy and TLS ------------------------------------------------

# Enable TLS 1.2+ (required by GitHub, older PowerShell defaults to TLS 1.0)
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor [Net.SecurityProtocolType]::Tls13

# Detect proxy: explicit env var > system default proxy
$ProxyArgs = @{}
$ProxyUrl = if ($env:HTTPS_PROXY) { $env:HTTPS_PROXY }
             elseif ($env:HTTP_PROXY) { $env:HTTP_PROXY }
             else { $null }

if ($ProxyUrl) {
    $ProxyArgs = @{ Proxy = $ProxyUrl; ProxyUseDefaultCredentials = $true }
    Write-Host "Using proxy: $ProxyUrl"
} else {
    # Try system default proxy (IE/WinHTTP settings)
    $DefaultProxy = [System.Net.WebRequest]::GetSystemWebProxy()
    $TestUri = [Uri]'https://api.github.com'
    $ResolvedProxy = $DefaultProxy.GetProxy($TestUri)
    if ($ResolvedProxy -and $ResolvedProxy.AbsoluteUri -ne $TestUri.AbsoluteUri) {
        $ProxyArgs = @{ Proxy = $ResolvedProxy.AbsoluteUri; ProxyUseDefaultCredentials = $true }
        Write-Host "Using system proxy: $($ResolvedProxy.AbsoluteUri)"
    }
}

$Repo = 'yijunyu/cargo-slicer'
$InstallDir = if ($env:CARGO_HOME) { Join-Path $env:CARGO_HOME 'bin' } else { Join-Path $HOME '.cargo\bin' }

# -- Detect platform --------------------------------------------------------

$Arch = $env:PROCESSOR_ARCHITECTURE
switch ($Arch) {
    'AMD64'  { $ArchLabel = 'x86_64' }
    'ARM64'  { $ArchLabel = 'aarch64' }
    default  {
        Write-Error "Unsupported architecture: $Arch`ncargo-slicer supports x86_64 and ARM64."
        exit 1
    }
}

# Try MSVC first, fall back to GNU (cross-compiled from Linux)
$TargetMsvc = "$ArchLabel-pc-windows-msvc"
$TargetGnu  = "$ArchLabel-pc-windows-gnu"
$Target = $TargetMsvc
$Archive = "cargo-slicer-$Target.zip"

Write-Host 'cargo-slicer installer'
Write-Host '======================'
Write-Host "Platform: $Target"
Write-Host "Install:  $InstallDir"
Write-Host ''

# -- Check prerequisites ----------------------------------------------------

if (-not (Get-Command rustup -ErrorAction SilentlyContinue)) {
    Write-Error "rustup not found.`nInstall Rust first: https://rustup.rs"
    exit 1
}

# -- Find latest release -----------------------------------------------------

Write-Host 'Finding latest release...'

$ReleaseUrl = "https://api.github.com/repos/$Repo/releases/latest"
try {
    $Release = Invoke-RestMethod -Uri $ReleaseUrl -Headers @{ 'User-Agent' = 'cargo-slicer-installer' } @ProxyArgs
} catch {
    Write-Host ''
    Write-Host "Could not fetch release info from GitHub." -ForegroundColor Red
    Write-Host "Error: $_" -ForegroundColor Red
    Write-Host ''
    if (-not $ProxyUrl) {
        Write-Host 'If you are behind a corporate proxy, set:' -ForegroundColor Yellow
        Write-Host '  $env:HTTPS_PROXY = "http://proxy.company.com:8080"'
        Write-Host '  irm https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.ps1 -Proxy $env:HTTPS_PROXY | iex'
        Write-Host ''
    }
    Write-Host "Or build from source:"
    Write-Host "  cargo install --git https://github.com/$Repo"
    exit 1
}

$Tag = $Release.tag_name
$Asset = $Release.assets | Where-Object { $_.name -eq $Archive }

if (-not $Tag) {
    Write-Error 'No releases found.'
    exit 1
}

Write-Host "Release: $Tag"
Write-Host "Archive: $Archive"
Write-Host ''

if (-not $Asset) {
    # Fall back to GNU target (cross-compiled from Linux)
    $Target = $TargetGnu
    $Archive = "cargo-slicer-$Target.zip"
    $Asset = $Release.assets | Where-Object { $_.name -eq $Archive }
    if ($Asset) {
        Write-Host "Using GNU build: $Archive"
    }
}

if (-not $Asset) {
    Write-Host "Precompiled binary for Windows is not available yet." -ForegroundColor Yellow
    Write-Host ''
    Write-Host 'Build from source instead:'
    Write-Host "  cargo install --git https://github.com/$Repo"
    Write-Host "  cargo +nightly install --git https://github.com/$Repo ``"
    Write-Host '    --profile release-rustc --features rustc-driver `'
    Write-Host '    --bin cargo-slicer-rustc --bin cargo_slicer_dispatch'
    exit 1
}

$DownloadUrl = $Asset.browser_download_url

# -- Download and extract ----------------------------------------------------

$TmpDir = Join-Path ([System.IO.Path]::GetTempPath()) "cargo-slicer-install-$([System.Guid]::NewGuid().ToString('N').Substring(0,8))"
New-Item -ItemType Directory -Path $TmpDir -Force | Out-Null

try {
    $ZipPath = Join-Path $TmpDir $Archive

    Write-Host 'Downloading...'
    Invoke-WebRequest -Uri $DownloadUrl -OutFile $ZipPath -UseBasicParsing @ProxyArgs

    Write-Host 'Extracting...'
    Expand-Archive -Path $ZipPath -DestinationPath $TmpDir -Force

    # -- Install binaries ----------------------------------------------------

    if (-not (Test-Path $InstallDir)) {
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
    }

    $PackageDir = Join-Path $TmpDir 'cargo-slicer'
    $Binaries = @('cargo-slicer.exe', 'cargo-slicer-rustc.exe', 'cargo_slicer_dispatch.exe', 'cargo-slicer.ps1')

    foreach ($bin in $Binaries) {
        $src = Join-Path $PackageDir $bin
        if (Test-Path $src) {
            Copy-Item $src -Destination (Join-Path $InstallDir $bin) -Force
            Write-Host "  Installed: $(Join-Path $InstallDir $bin)"
        } else {
            Write-Warning "$bin not found in archive"
        }
    }

    Write-Host ''

    # -- Build cargo-slicer-rustc from source if not in archive ---------------

    if (-not (Test-Path (Join-Path $InstallDir 'cargo-slicer-rustc.exe'))) {
        Write-Host 'cargo-slicer-rustc.exe not in prebuilt archive (must be built natively).'
        Write-Host 'Building from source (this may take a few minutes)...'
        Write-Host ''
        & cargo +nightly install --git "https://github.com/$Repo" `
            --profile release-rustc --features rustc-driver `
            --bin cargo-slicer-rustc
        if ($LASTEXITCODE -eq 0) {
            Write-Host '  Built: cargo-slicer-rustc.exe'
        } else {
            Write-Host ''
            Write-Host 'Failed to build cargo-slicer-rustc.exe automatically.' -ForegroundColor Yellow
            Write-Host 'Build it manually:'
            Write-Host "  cargo +nightly install --git https://github.com/$Repo ``"
            Write-Host '    --profile release-rustc --features rustc-driver `'
            Write-Host '    --bin cargo-slicer-rustc'
        }
    }
} finally {
    Remove-Item -Recurse -Force $TmpDir -ErrorAction SilentlyContinue
}

# -- Ensure nightly toolchain -----------------------------------------------

$NightlyCheck = & rustup run nightly rustc --version 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host 'Installing Rust nightly toolchain...'
    & rustup toolchain install nightly
    Write-Host ''
}

$NightlyVer = & rustup run nightly rustc --version 2>&1
Write-Host "Nightly: $NightlyVer"

# -- Verify PATH -------------------------------------------------------------

if ($env:PATH -split ';' -notcontains $InstallDir) {
    Write-Host ''
    Write-Host "Warning: $InstallDir is not in your PATH." -ForegroundColor Yellow
    Write-Host 'Add it with:'
    Write-Host "  `$env:PATH = `"$InstallDir;`$env:PATH`""
    Write-Host 'Or permanently via System > Environment Variables.'
}

# -- Print usage -------------------------------------------------------------

Write-Host ''
Write-Host '============================================'
Write-Host '  cargo-slicer installed successfully!'
Write-Host '============================================'
Write-Host ''
Write-Host 'Quick start - accelerate any Rust project:'
Write-Host ''
Write-Host '  cargo-slicer.ps1 C:\path\to\your\project'
Write-Host ''
Write-Host 'Or manually:'
Write-Host ''
Write-Host '  cd C:\path\to\your\project'
Write-Host '  cargo-slicer pre-analyze'
Write-Host '  $env:CARGO_SLICER_VIRTUAL=1'
Write-Host '  $env:CARGO_SLICER_CODEGEN_FILTER=1'
Write-Host '  $env:RUSTC_WRAPPER=(Get-Command cargo_slicer_dispatch).Source'
Write-Host '  cargo +nightly build --release'
Write-Host ''
Write-Host "Documentation: https://github.com/$Repo"
