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

plot.metric <- function(the.list, name, ndiffs, w.sizes, out.dir){
  png(filename = file.path(out.dir, paste0(name, ".png"))
  plot(NULL, xlim = range(w.sizes), ylim = range(the.list))
  for(ndiff in ndiffs){
    lines(w.sizes, the.list[[as.character(ndiff)]])
  }
  garbage <- dev.off()
}

cat("period","w.size","ndiff","nwind","p.nval","p.nnorm","RMSE",paste(levels, "conf"),"\n", sep="\t")
for(period in periods){
  p.nonevalid.l <- list()
  p.nonnormal.l <- list()
  rsme.l <- list()
  
  for(ndiff in ndiffs){
    
    p.nonevalid.v <- NULL
    p.nonnormal.v <- NULL
    rsme.v <- NULL
    
    pre.results <- pre.modeling(issues.file = opts$ISSUES_FILE,
      sampling.period = period, ndiff = ndiff, out.dir = opts$out.dir,
      start.date = start.date, end.date = end.date)
    if(!opts$forcediff){
      ndiff <- pre.results$ndiff
    }
    for(w.size in w.sizes){
      results <- model.regime(pre.results$ts, window.size = w.size, conf.levels = levels,
        ndiff = ndiff, normality.signif = normality.signif, verbose = verbose)
      
      p.nonevalid <- results$n.nonevalid / results$n.windows
      p.nonnormal <- results$n.nonnormal / (results$n.windows - results$n.nonevalid)
      p.inconf <- as.numeric(results$n.inconf / (results$n.inconf + results$n.outconf))
      rmse <- sqrt(mean(results$fc.errs^2))
      
      p.nonevalid.v <- append(p.nonevalid.v, p.nonevalid)
      p.nonnormal.v <- append(p.nonnormal.v, p.nonnormal)
      rsme.v <- append(rsme.v, rsme)
      
      cat(period, w.size, ndiff, results$n.windows, p.nonevalid, 
          p.nonnormal, round(rmse,4), round(p.inconf,4), "\n", sep="\t")
    }
    
    p.none.valid.l[[as.character(ndiff)]] <- p.none.valid.v
    p.non.normal.l[[as.character(ndiff)]] <- p.non.normal.v
    rsme.l[[as.character(ndiff)]] <- rsme.v
  }
  
  if(!is.null(out.dir)){
    plot.metric(p.nonevalid.l, w.sizes = w.sizes, ndiffs = ndiffs, 
                name = "p.nonevalid", out.dir = out.dir)    
    plot.metric(p.nonnormal.l, w.sizes = w.sizes, ndiffs = ndiffs, 
                name = "p.nonnormal", out.dir = out.dir)
    plot.metric(rsme.l, w.sizes = w.sizes, ndiffs = ndiffs, 
                name = "RSME", out.dir = out.dir)
  }
}
# 
# cat("\nPercent of forecasts within prediction interval:\n")
# cat(names(pcts), "\n")
# cat(paste0(sprintf("%0.4f", pcts*100), "%"), "\n")