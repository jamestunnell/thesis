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
  ISSUES_FILE = "~/../thesis/data/mongodb/issues.txt",
  outdir = "~/../thesis",
  srcdir = "~/../thesis/src",
  period = "7"
)

issues.file = opts$ISSUES_FILE
sampling.period = as.integer(opts$period)
src.dir <- opts$srcdir
out.dir <- opts$outdir

library(dse)
library(xts)

source(file.path(src.dir,"sampling.R"))
source(file.path(src.dir,"testing.R"))
source(file.path(src.dir,"modeling.R"))
source(file.path(src.dir,"plotting.R"))
source(file.path(src.dir,"forecasting.R"))

issues <- read.table(issues.file, header = T)
s <- sample.issues.all(issues, sampling.period)
ts <- as.xts(data.frame(Bugs=s$bugs, Improvements=s$imps, Features=s$news), s$date)

cat("=========================================\n")
cat("             Pre-Modeling\n")
cat("=========================================\n\n")

cat("Plotting time-series\n")
fname <- file.path(out.dir, "time_series.png")
ts.plot(ts, fname)

ST_TYPE = "constant"

fname <- file.path(out.dir, "stationarity.txt")
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
  fname <- file.path(out.dir, "time_series_diff.png")
  ts.plot(ts[2:nrow(ts)], fname)
  cat("\n")
}

n.sample.per <- 78
n.windows <- floor(nrow(s) / n.sample.per)

labs <- list(bugs = names(ts)[pmatch("Bug",names(ts))],
             imps = names(ts)[pmatch("Imp",names(ts))],
             news = names(ts)[pmatch("Fea",names(ts))])

imps.hypoth = min(na.trim(ts[,labs$imps])):max(na.trim(ts[,labs$imps]))
news.hypoth = min(na.trim(ts[,labs$news])):max(na.trim(ts[,labs$news]))

threepoints <- function(x){
  y <- sort(x)
  n <- length(y)
  return(c(y[1], y[floor(n/2)], y[n]))
}

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
  
  ts.data <- TSdata(
    output = as.matrix(ts.sub[,labs$bugs]),
    input = as.matrix(ts.sub[,c(labs$imps,labs$news)])
  )
  model <- modeling.methodology(ts.data)
  
  cat("Plotting one-step ahead predictions\n")
  fname <- file.path(out.dir, paste0("one-step_predictions_", s.min, "-", s.max, ".png"))
  plot.predictions(model, s.range, fname, n.plots = 1, width = 1200, height.per = 400, cex = 1.35)
  
#   cat("Forecasting for hypothetical future exogenous values (one-step only)\n")
#   fname <- file.path(out.dir, paste0("forecast_hypotheticals_mean3d_", s.min, "-", s.max, ".png"))
#   forecast.hypotheticals.mean3d(model, data.base=ts.data, fname=fname,
#                                 imps.hypoth=imps.hypoth,news.hypoth=news.hypoth)
  
  imps.actual <- ts[s.max+1,labs$imps]
  news.actual <- ts[s.max+1,labs$news]
  fname <- file.path(out.dir, paste0("forecast_hypotheticals_conf2d_", s.min, "-", s.max, ".png"))
  forecast.hypotheticals.conf2d(model, data.base=ts.data, ci=0.8, fname=fname,
                                imps.actual=imps.actual, news.actual=news.actual,
                                imps.hypoth=imps.hypoth, news.hypoth=news.hypoth)
}