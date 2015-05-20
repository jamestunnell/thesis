#!/usr/bin/env Rscript

'Model software defects using time series model of historical data. 
Runs multiple times, once for each combination of parameters.

usage:
model.R ISSUES_FILE [options]

arguments:
ISSUES_FILE  A text file, containing CSV-like table, with software issue data.

options:
--outdir=O      Path to a directory where plots can be saved as files
--periods=P     Sampling period, in days [default: 7,14,30]
--ndiffs=D      # of differences to take, for non-stationary time series data [default: 1,2]
--windows=W     Sample window size [default: 24,36,48]
--normsignif=N  Alpha level to use in normality test of model residuals [default: 0.05]
--levels=L      Confidence levels (1-99), for forecast testing [default: 75,90]
--install       Before execution, install defectPrediction package from GitHub
--verbose       Enable verbose mode
--startdate=S   Date to start time series
--enddate=E     Date to end time series
--forcediff     Force the time series data to be differenced, whether it is stationary or not
' -> doc

library(docopt)
opts <- docopt(doc) # retrieve the command-line arguments
# opts <- list(
#   ISSUES_FILE = "~/projects/thesis/data/mongodb/issues.txt",
#   periods = "30",
#   ndiffs = "1",
#   windows = "20",
#   normsignif = "0.05",
#   levels = "75,90",
#   install = F,
#   outdir = NULL,
#   start.date = NULL,
#   end.date = NULL,
#   verbose = FALSE
# )

issues.file = opts$ISSUES_FILE
periods = as.integer(unlist(strsplit(opts$period, split=",")))
w.sizes <- as.integer(unlist(strsplit(opts$window, split=",")))
ndiffs <- as.integer(unlist(strsplit(opts$ndiffs, split=",")))
levels <- as.integer(unlist(strsplit(opts$levels, split=",")))
out.dir <- opts$outdir
verbose <- opts$verbose
start.date <- opts$startdate
end.date <- opts$enddate
normality.signif <- as.numeric(opts$normsignif)

levels <- rev(sort(levels))

if(opts$install){
  library(devtools)
  install_github("jamestunnell/defectPrediction")  
}
library(defectPrediction)

cat("period","w.size","ndiff","nwind","nnval","nnnorm","RMSE",paste(levels, "conf"),"\n", sep="\t")
for(period in periods){
  for(ndiff in ndiffs){
    pre.results <- pre.modeling(issues.file = opts$ISSUES_FILE,
      sampling.period = period, ndiff = ndiff, out.dir = opts$out.dir,
      start.date = start.date, end.date = end.date)
    if(!opts$forcediff){
      ndiff <- pre.results$ndiff
    }
    for(w.size in w.sizes){
      results <- model.regime(pre.results$ts, window.size = w.size, 
        conf.levels = levels, ndiff = ndiff, normality.signif = normality.signif,
        out.dir = opts$outdir, verbose = verbose)
      p.inconf <- as.numeric(results$n.inconf / (results$n.inconf + results$n.outconf))
      rmse <- sqrt(mean(results$fc.errs^2))
      cat(period, w.size, ndiff, results$n.windows, results$n.nonevalid, 
          results$n.nonnormal, round(rmse,4), round(p.inconf,4), "\n", sep="\t")
    }
  }
}
# 
# cat("\nPercent of forecasts within prediction interval:\n")
# cat(names(pcts), "\n")
# cat(paste0(sprintf("%0.4f", pcts*100), "%"), "\n")