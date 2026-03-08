# cargo-slicer.ps1 — Drop-in accelerated Rust build (Windows)
#
# Usage:
#   cargo-slicer.ps1                          # build --release in current dir
#   cargo-slicer.ps1 C:\path\to\project       # build a specific project
#   cargo-slicer.ps1 . --features foo         # extra args passed to cargo build
#
# Prerequisites:
#   - Rust nightly toolchain:  rustup toolchain install nightly
#   - cargo-slicer installed:  cargo install --path \path\to\cargo-slicer
#   - Driver installed:        cargo +nightly install --path \path\to\cargo-slicer `
#                                 --profile release-rustc `
#                                 --bin cargo-slicer-rustc --bin cargo_slicer_dispatch `
#                                 --features rustc-driver

$ErrorActionPreference = 'Stop'

# -- Parse arguments ---------------------------------------------------------

$ProjectDir = '.'
$ExtraArgs = @()

if ($args.Count -ge 1 -and (Test-Path $args[0] -PathType Container)) {
    $ProjectDir = $args[0]
    $ExtraArgs = $args[1..($args.Count - 1)]
} elseif ($args.Count -ge 1) {
    $ExtraArgs = $args
}

$ProjectDir = (Resolve-Path $ProjectDir).Path

if (-not (Test-Path (Join-Path $ProjectDir 'Cargo.toml'))) {
    Write-Error "No Cargo.toml found in $ProjectDir"
    exit 1
}

# -- Locate binaries ---------------------------------------------------------

$Dispatch = Get-Command cargo_slicer_dispatch -ErrorAction SilentlyContinue
$Driver = Get-Command cargo-slicer-rustc -ErrorAction SilentlyContinue
$Slicer = Get-Command cargo-slicer -ErrorAction SilentlyContinue

if (-not $Dispatch) {
    Write-Error @"
cargo_slicer_dispatch not found in PATH.
Install: cargo +nightly install --path <cargo-slicer-dir> --profile release-rustc ``
           --bin cargo_slicer_dispatch --bin cargo-slicer-rustc --features rustc-driver
"@
    exit 1
}
if (-not $Driver) {
    Write-Error 'cargo-slicer-rustc not found in PATH.'
    exit 1
}
if (-not $Slicer) {
    Write-Error "cargo-slicer not found in PATH.`nInstall: cargo install --path <cargo-slicer-dir>"
    exit 1
}

# Verify nightly is available
$null = & rustup run nightly rustc --version 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Error "Rust nightly toolchain not installed.`nInstall: rustup toolchain install nightly"
    exit 1
}

# -- Step 1: Pre-analysis (cross-crate call graph) --------------------------

Write-Host '=== Step 1/3: Pre-analyzing cross-crate call graph ==='
Push-Location $ProjectDir
try {
    & $Slicer.Source pre-analyze
    if ($LASTEXITCODE -ne 0) { throw 'pre-analyze failed' }

    # -- Step 2: Build with virtual slicing ----------------------------------

    Write-Host ''
    Write-Host '=== Step 2/3: Building with virtual slicing (codegen filtering) ==='

    $env:CARGO_SLICER_VIRTUAL = '1'
    $env:CARGO_SLICER_CODEGEN_FILTER = '1'
    $env:CARGO_SLICER_DRIVER = $Driver.Source
    $env:RUSTC_WRAPPER = $Dispatch.Source

    $sw = [System.Diagnostics.Stopwatch]::StartNew()
    & cargo +nightly build --release @ExtraArgs
    $sw.Stop()

    if ($LASTEXITCODE -ne 0) { throw 'cargo build failed' }

    # -- Step 3: Summary -----------------------------------------------------

    Write-Host ''
    Write-Host '=== Step 3/3: Done ==='
    Write-Host "Project:  $ProjectDir"
    Write-Host "Time:     $($sw.Elapsed.ToString('mm\:ss\.fff'))"

    $ReleaseDir = Join-Path $ProjectDir 'target\release'
    $Exes = Get-ChildItem -Path $ReleaseDir -Filter '*.exe' -ErrorAction SilentlyContinue |
            Select-Object -First 5 -ExpandProperty FullName
    if ($Exes) {
        Write-Host "Binary:   $($Exes -join ', ')"
    }

    # Show cache stats if debug was enabled
    $DebugLog = Join-Path $ProjectDir '.cargo-slicer-debug.log'
    if (Test-Path $DebugLog) {
        $Stubbed = (Select-String -Path $DebugLog -Pattern '^\[codegen-filter\] STUB:' -SimpleMatch).Count
        Write-Host "Stubbed:  $Stubbed functions"
    }
} finally {
    Pop-Location
    # Clean up env vars
    Remove-Item env:CARGO_SLICER_VIRTUAL -ErrorAction SilentlyContinue
    Remove-Item env:CARGO_SLICER_CODEGEN_FILTER -ErrorAction SilentlyContinue
    Remove-Item env:CARGO_SLICER_DRIVER -ErrorAction SilentlyContinue
    Remove-Item env:RUSTC_WRAPPER -ErrorAction SilentlyContinue
}
