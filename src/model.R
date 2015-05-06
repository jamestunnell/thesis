#!/usr/bin/env Rscript

'Model software defects using ARIMA time series model of historical data.

usage:
model.R ISSUES_FILE [options]

arguments:
ISSUES_FILE  A text file, containing CSV-like table, with software issue data.

options:
--outdir=O  Path to a directory where plots can be saved as files
--period=P  Sampling period, in days [default: 7]
--ndiffs=D  # of differences to take, for non-stationary time series data [default: 1]
--window=W  Sample window size [default: 78]
--install   Before execution, install defectPrediction package from GitHub
' -> doc

library(docopt)
opts <- docopt(doc) # retrieve the command-line arguments
# opts <- list(
#   ISSUES_FILE = "~/projects/thesis/data/mongodb/issues.txt",
#   outdir = "~/projects/thesis",
#   srcdir = "~/projects/thesis/src",
#   period = "30",
#   ndiffs = "1",
#   window = "20"
# )

issues.file = opts$ISSUES_FILE
sampling.period = as.integer(opts$period)
src.dir <- opts$srcdir
out.dir <- opts$outdir
ndiffs <- as.integer(opts$ndiffs)
w.size <- as.integer(opts$window)

if(opts$install){
  library(devtools)
  install_github("jamestunnell/defectPrediction")  
}
library(defectPrediction)

pcts <- model.regime(
  issues.file = opts$ISSUES_FILE,
  sampling.period = as.integer(opts$period),
  window.size = as.integer(opts$window),
  ndiffs = as.integer(opts$ndiffs),
  out.dir = opts$outdir
)

cat("\nPercent of forecasts within prediction interval:\n")
cat(names(pcts), "\n")
cat(paste0(sprintf("%0.4f", pcts*100), "%"), "\n")