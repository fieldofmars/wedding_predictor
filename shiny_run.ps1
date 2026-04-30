# shiny_run.ps1
# Script to run the Shiny app for the Wedding Predictor project

$Rscript = "C:\Program Files (x86)\R\R-4.6.0\bin\x64\Rscript.exe"
$libPath = "$env:USERPROFILE\Documents\R\win-library\4.6"

# Set the R_LIBS_USER environment variable for the R session
$env:R_LIBS_USER = $libPath

# Run the Shiny app on port 8100
Write-Host "Starting Shiny app on http://127.0.0.1:8100 ..."
& $Rscript -e "shiny::runApp('.', port = 8100, launch.browser = FALSE)"
