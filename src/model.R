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
# opts <- docopt(doc) # retrieve the command-line arguments
opts <- list(
  ISSUES_FILE = "C:/Users/James/thesis/data/mongodb/issues.txt",
  outdir = "C:/Users/James/thesis/",
  srcdir = "C:/Users/James/thesis/src/",
  period = "7"
)

issues.file = opts$ISSUES_FILE
sampling.period = as.integer(opts$period)
src.dir <- normalizePath(opts$srcdir)
out.dir <- normalizePath(opts$outdir)

library(dse)
library(xts)
source(paste0(src.dir,"sampling.R"))
source(paste0(src.dir,"testing.R"))
source(paste0(src.dir,"modeling.R"))
source(paste0(src.dir,"plotting.R"))
source(paste0(src.dir,"tsdata.R"))

issues <- read.table(issues.file, header = T)
s <- sample.issues.all(issues, sampling.period)
ts <- as.xts(data.frame(Bugs=s$bugs, Improvements=s$imps, Features=s$news), s$date)

cat("=========================================\n")
cat("             Pre-Modeling\n")
cat("=========================================\n\n")

cat("Plotting time-series\n")
fname <- paste0(out.dir, "time_series.png")
ts.plot(ts, fname)

ST_TYPE = "constant"

fname <- paste0(out.dir, "stationarity.txt")
cat("", file = fname, append = F)
diff.any = F
for(i in 1:ncol(ts)){
  st <- test.stationarity(ts[,i], type = ST_TYPE, df.level = 1, kpss.level = 10)
  print.stationarity(st, names(ts)[i], fname)
  if(!st$df$stationary | !st$kpss$stationary){
    diff.any <- T
    ts[,i] <- diff(ts[,i])
    names(ts)[i] <- paste(names(ts)[i],"(Difference)")
    st <- test.stationarity(ts[2:nrow(ts),i], type = ST_TYPE, df.level = 1, kpss.level = 10)
    print.stationarity(st, names(ts)[i], fname)
    stopifnot(st$df$stationary & st$kpss$stationary)
  }
}

if(diff.any){
  #ts <- ts[2:nrow(ts)]
  cat("Plotting differenced time-series\n")
  fname <- paste0(out.dir, "time_series_diff.png")
  ts.plot(ts[2:nrow(ts)], fname)
  cat("\n")
}

n.sample.per <- 78
n.windows <- floor(nrow(s) / n.sample.per)

for(w in 1:n.windows){
  s.min <- (w-1)*n.sample.per+1
  s.max <- w*n.sample.per
  if(diff.any){
    s.min <- s.min+1
    s.max <- s.max+1
  }
  s.range <- s.min:s.max
  
  ts.sub <- ts[s.range]

  cat("=========================================\n")
  cat("        Modeling samples", s.min, "to", s.max, "\n")
  cat("=========================================\n\n")
  
  labs <- list(bugs = names(ts)[pmatch("Bug",names(ts))],
               imps = names(ts)[pmatch("Imp",names(ts))],
               news = names(ts)[pmatch("Fea",names(ts))])
  ts.data <- TSdata(
    output = as.matrix(ts.sub[,labs$bugs]),
    input = as.matrix(ts.sub[,c(labs$imps,labs$news)])
  )
  model <- modeling.methodology(ts.data)
  
  cat("Plotting one-step ahead predictions\n")
  fname <- paste0(out.dir, s.min, "-", s.max, "_one-step_predictions.png")
  plot.predictions(model, s.range, fname, n.plots = 1, width = 1200, height.per = 400, cex = 1.35)
  
  cat("Forecasting for hypothetical future exogenous values (one-step only)\n")
  # TODO
  #    forecast.hypotheticals.onestep(model, , 
  #      imps.hypoth = min(s$imps):max(s$imps), 
  #      news.hypoth = min(s$news):max(s$news),
  #      plot.2d = T)
}