#!/usr/bin/env Rscript

'Model software defects using ARIMA time series model of historical data.

usage:
model.R ISSUES_FILE [options]

arguments:
ISSUES_FILE  A text file, containing CSV-like table, with software issue data.

options:
--srcdir=S  Path where modeling scripts are located [default: ./]
--outdir=O  Path to a directory where plots can be saved as files [default: ./]
--period=P  Sampling period, in days [default: 7]
' -> doc

library(docopt)
opts <- docopt(doc) # retrieve the command-line arguments
# opts <- list(
#   ISSUES_FILE = "~/projects/thesis/data/mongodb/issues.txt",
#   outdir = "~/projects/thesis/proposal/images/",
#   srcdir = "~/projects/thesis/modeling/",
#   period = "7"
# )

issues.file = opts$ISSUES_FILE
sampling.period = as.integer(opts$period)
src.dir <- opts$srcdir
out.dir <- opts$outdir

library(dse)
source(paste0(src.dir,"sampling.R"))
source(paste0(src.dir,"testing.R"))
source(paste0(src.dir,"modeling.R"))
source(paste0(src.dir,"plotting.R"))

issues <- read.table(issues.file, header = T)
s <- sample.issues.all(issues, sampling.period)

bc <- s$bugs.created
ir <- s$imps.resolved
nr <- s$news.resolved

tseries = list("Bugs Created"=bc, "Improvements Resolved"=ir, "New Features Resolved"=nr)
diff = c(F,F,F)

cat("=========================================\n")
cat("             Pre-Modeling\n")
cat("=========================================\n\n")

cat("Plotting time-series\n")
fname <- paste0(out.dir, "time_series.png")
plot.tseries(tseries, fname, diff = diff)

ST_TYPE = "constant"

fname <- paste0(out.dir, "stationarity.txt")
cat("", file = fname, append = F)
for(i in 1:length(tseries)){
  y <- tseries[[i]]
  name <- names(tseries)[i]
  st <- test.stationarity(y, type = ST_TYPE, df.level = 1, kpss.level = 10)
  print.stationarity(st, name, fname, diff = F)
  if(!st$df$stationary | !st$kpss$stationary){
    diff[[i]] <- T
    y <- tseries[[i]] <- diff(y)
    st <- test.stationarity(y, type = ST_TYPE, df.level = 1, kpss.level = 10)
    print.stationarity(st, name, fname, diff = T)
    stopifnot(st$df$stationary & st$kpss$stationary)
  }
}

if(any(diff)){
  for(i in 1:length(tseries)){
    if(diff[i]){
      cat("Differenced", names(tseries)[i],"\n")
    } else {
      tseries[[i]] <- tseries[[i]][2:length(tseries[[i]])]
    }
  }
  cat("Plotting differenced time-series\n")
  fname <- paste0(out.dir, "time_series_diff.png")
  plot.tseries(tseries, fname, diff = diff)
  cat("\n")
}

n.sample.per <- 78
n.windows <- floor(length(tseries[["Bugs Created"]]) / n.sample.per)

for(w in 1:n.windows){
  s.min <- (w-1)*n.sample.per+1
  s.max <- w*n.sample.per
  s.range <- s.min:s.max
  
  ts <- TSdata(output = matrix(tseries[["Bugs Created"]][s.range],byrow=F,ncol=1),
               input = matrix(c(tseries[["Improvements Resolved"]][s.range],tseries[["New Features Resolved"]][s.range]),byrow=F,ncol=2))

  cat("=========================================\n")
  cat("        Modeling samples", s.min, "to", s.max, "\n")
  cat("=========================================\n\n")
  
  model <- modeling.methodology(ts)
  
  cat("Plotting one-step ahead predictions\n")
  fname <- paste0(out.dir, s.min, "-", s.max, "_one-step_predictions.png")
  plot.predictions(model, fname, diff[1], n.plots = 1, width = 1200, height.per = 400, cex = 1.35)
}