#!/usr/bin/env Rscript


'Forecast software defects using time series model of historical data.

usage:
forecast.R ISSUES_FILE MD_START FC_START N_FC [options]

arguments:
ISSUES_FILE   A text file, containing CSV-like table, with bug data from all releases.
MD_START   Date to start modeling (YYYY-MM-DD)
FC_START      Date to start forecasting (YYYY-MM-DD)
N_FC          Forecast horizon (number of periods to predict)

options:
--outdir=O  Path to a directory where results can be saved as files [default: ./]
--width=W   Width of output images [default: 1200]
--height=H  Height of output images [default: 600]
--conf=C    Forecast confidence level(s) (0-100, comma-separated) [default: 80,95]
--period=P  Sample period (in days) [default: 7]
--log       Send text output to a log file
' -> doc

library(docopt) # load the docopt library
source("~/projects/thesis/modeling/sample.R")
source("~/projects/thesis/modeling/arima.R")

# opts <- docopt(doc) # retrieve the command-line arguments
opts <- list(
  next_release = T, date_range = F,
  ISSUES_FILE = "~/projects/thesis/data/mongodb.txt",
  MD_START = "2013-06-01",
  FC_START = "2014-11-01", N_FC = 4,
  outdir = "~/projects/thesis/", width = 1200, height = 900,
  conf = "80,95", period = 7, log = F
)

issues_file <- opts$ISSUES_FILE
outdir <- normalizePath(paste0(opts$outdir,"/"))
if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}
W <- as.integer(opts$width)
H <- as.integer(opts$height)
conf <- as.numeric(strsplit(opts$conf,",")[[1]])

issues <- read.table(issues_file, header = TRUE)
  
require(timeDate)
require(stats)

period <- as.numeric(opts$period)
nfc <- as.numeric(opts$N_FC)
md_start.date <- as.Date(opts$MD_START)
fc_start.date <- as.Date(opts$FC_START)
fc_stop.date <- fc_start.date + period * nfc

issues$created <- timeDate(issues$created)
issues$resolved <- timeDate(issues$resolved)
s <- sample.dateRange(issues,md_start.date,fc_stop.date,period)

ns <- nrow(s)
fc <- arima.forecast(s$imps.resolved,s$news.resolved,
                        head(s$bugs.created,ns-nfc), conf)

Box.test(fc$model$residuals, type = "Ljung-Box", lag = 10)

fname <- paste0(outdir,"/","Model Diagnostic.png")
png(filename = fname, width = W, height = H)
tsdiag(fc$model)
garbage <- dev.off()

fname <- paste0(outdir,"/","Forecasting.png")
png(filename = fname, width = W, height = H)
main <- paste0("Forecasting Active Bugs using ", fc$method, "\nwith ",
               paste(fc$level,collapse="%, "), "% confidence")
plot(fc,main=main, ylim=range(c(0),s$bugs.created))
lines((ns-nfc):ns,tail(s$bugs.created,nfc+1),lty=2)
lines(head(s$bugs.created,ns-nfc) - fc$model$residuals, col="blue")
garbage <- dev.off()