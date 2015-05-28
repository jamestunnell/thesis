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
--install       Before execution, install defectPrediction package from GitHub
--verbose       Enable verbose mode
--startdate=S   Date to start time series
--enddate=E     Date to end time series
--forcediff     Force the time series data to be differenced, whether it is stationary or not
' -> doc

library(docopt)
opts <- docopt(doc) # retrieve the command-line arguments
# opts <- list(
#   ISSUES_FILE = "~/projects/thesis/data/mongodb_coreserver_issues.txt",
#   periods = "30",
#   ndiffs = "1,2",
#   windows = "12,15,18,21,24,27,30",
#   normsignif = "0.05",
#   #levels = "75,90",
#   install = F,
#   outdir = "~/projects/thesis/runs",
#   start.date = NULL,
#   end.date = NULL,
#   verbose = F,
#   forcediff = T
# )

issues.file = opts$ISSUES_FILE
periods = as.integer(unlist(strsplit(opts$period, split=",")))
w.sizes <- as.integer(unlist(strsplit(opts$window, split=",")))
ndiffs <- as.integer(unlist(strsplit(opts$ndiffs, split=",")))
levels <- c(75,90) #as.integer(unlist(strsplit(opts$levels, split=",")))
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

plot.metric <- function(the.list, name, period, ndiffs, w.sizes, out.dir){
  plot.colors <- rainbow(length(ndiffs))
  
  png(filename = file.path(out.dir, paste0(name, "_", period, ".png")))
  plot(NULL, xlim = range(w.sizes), ylim = range(the.list), xlab = "window size", ylab = name)
  for(i in 1:length(ndiffs)){
    ndiff <- ndiffs[i]
    lines(w.sizes, the.list[[as.character(ndiff)]], col = plot.colors[i])
  }
  garbage <- dev.off()
}

cat("period","w.size","ndiff","nwind","p.nval","p.nnorm","RMSE",paste(levels, "conf"),"\n", sep="\t")
for(period in periods){
  p.nonevalid.l <- list()
  p.nonnormal.l <- list()
  rmse.l <- list()
  p.inconf.ninety.l <- list()
  p.inconf.seventyfive.l <- list()
  
  for(ndiff in ndiffs){
    
    p.nonevalid.v <- NULL
    p.nonnormal.v <- NULL
    rmse.v <- NULL
    p.inconf.ninety.v <- NULL
    p.inconf.seventyfive.v <- NULL
    
    pre.results <- pre.modeling(issues.file = opts$ISSUES_FILE,
      sampling.period = period, out.dir = out.dir,
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
      rmse.v <- append(rmse.v, rmse)
      p.inconf.ninety.v <- append(p.inconf.ninety.v, p.inconf[1])
      p.inconf.seventyfive.v <- append(p.inconf.seventyfive.v, p.inconf[2])
      
      cat(period, w.size, ndiff, results$n.windows, round(p.nonevalid, 4), 
          round(p.nonnormal,4), round(rmse,4), round(p.inconf,4), "\n", sep="\t")
    }
    
    key <- as.character(ndiff)
    p.nonevalid.l[[key]] <- p.nonevalid.v
    p.nonnormal.l[[key]] <- p.nonnormal.v
    rmse.l[[key]] <- rmse.v
    p.inconf.ninety.l[[key]] <- p.inconf.ninety.v
    p.inconf.seventyfive.l[[key]] <- p.inconf.seventyfive.v
  }
  
  if(!is.null(out.dir)){
    plot.metric(p.nonevalid.l, w.sizes = w.sizes, ndiffs = ndiffs, period = period, 
                name = "p.nonevalid", out.dir = out.dir)    
    plot.metric(p.nonnormal.l, w.sizes = w.sizes, ndiffs = ndiffs, period = period, 
                name = "p.nonnormal", out.dir = out.dir)
    plot.metric(rmse.l, w.sizes = w.sizes, ndiffs = ndiffs, period = period, 
                name = "rmse", out.dir = out.dir)
    plot.metric(p.inconf.ninety.l, w.sizes = w.sizes, ndiffs = ndiffs, period = period, 
                name = "90pct.conf", out.dir = out.dir)
    plot.metric(p.inconf.seventyfive.l, w.sizes = w.sizes, ndiffs = ndiffs, period = period, 
                name = "75pct.conf", out.dir = out.dir)
  }
}
# 
# cat("\nPercent of forecasts within prediction interval:\n")
# cat(names(pcts), "\n")
# cat(paste0(sprintf("%0.4f", pcts*100), "%"), "\n")