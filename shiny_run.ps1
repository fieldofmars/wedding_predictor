# shiny_run.ps1
# Script to run the Shiny app for the Wedding Predictor project

# --- Locate Rscript -----------------------------------------------------------
# Preferred: a known local install. Fallback: whatever `Rscript` is on PATH.
$RscriptCandidates = @(
    "C:\Program Files (x86)\R\R-4.6.1\bin\Rscript.exe",
    "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
)

$Rscript = $null
foreach ($candidate in $RscriptCandidates) {
    if (Test-Path $candidate) {
        $Rscript = $candidate
        break
    }
}

if (-not $Rscript) {
    $onPath = Get-Command Rscript.exe -ErrorAction SilentlyContinue
    if ($onPath) { $Rscript = $onPath.Source }
}

if (-not $Rscript) {
    Write-Error "Rscript not found. Install R, or add its 'bin' directory to PATH."
    exit 1
}

# NOTE: do NOT force R_LIBS_USER here. R's own default user library
# (%LOCALAPPDATA%\R\win-library\4.6) is where packages are installed; overriding it
# with the legacy ~/Documents/R/win-library path hides them and the app fails with
# "there is no package called 'pacman'".

Write-Host "Using Rscript: $Rscript"

# Run the Shiny app on port 8100
Write-Host "Starting Shiny app on http://127.0.0.1:8100 ..."
& $Rscript -e "shiny::runApp('.', port = 8100, launch.browser = FALSE)"
