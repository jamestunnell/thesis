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

# cat("=========================================\n")
# cat("             Pre-Modeling\n")
# cat("=========================================\n\n")

# cat("Plotting time-series\n")
fname <- file.path(out.dir, "time_series.png")
ts.plot(ts, fname)

ST_TYPE = "constant"

fname <- file.path(out.dir, "stationarity.txt")
cat("", file = fname, append = F)
needs.diffed = F
for(i in 1:ncol(ts)){
  st <- test.stationarity(ts[,i], type = ST_TYPE, df.level = 1, kpss.level = 10)
  print.stationarity(st, names(ts)[i], fname)
  if(!st$df$stationary | !st$kpss$stationary){
    needs.diffed <- T
  }
}

diffed = F
if(needs.diffed){
  diffed = T
  for(i in 1:ncol(ts)){
    ts[,i] <- diff(ts[,i])
    names(ts)[i] <- paste(names(ts)[i],"(Difference)")
    st <- test.stationarity(ts[2:nrow(ts),i], type = ST_TYPE, df.level = 1, kpss.level = 10)
    print.stationarity(st, names(ts)[i], fname)
    stopifnot(st$df$stationary & st$kpss$stationary)
  }

#   cat("Plotting differenced time-series\n")
  fname <- file.path(out.dir, "time_series_diff.png")
  ts.plot(ts[2:nrow(ts)], fname)
#   cat("\n")
}

w.size <- 120
#n.windows <- floor(nrow(s) / w.size)
ci <- c(0.975,0.95,0.9,0.8,0.75,0.5)

labs <- list(bugs = names(ts)[pmatch("Bug",names(ts))],
             imps = names(ts)[pmatch("Imp",names(ts))],
             news = names(ts)[pmatch("Fea",names(ts))])

ci.inout <- mat.or.vec(length(ci),2)
rownames(ci.inout) <- paste0(100*ci,"% conf")
colnames(ci.inout) <- c("in","out")
fc.errs <- NULL

for(w.start in (if(diffed){ 2 } else{ 1 }):(nrow(ts)-w.size)){
  s.min <- w.start
  s.max <- w.start + w.size - 1
  s.range <- s.min:s.max
  ts.sub <- ts[s.range]

#   cat("=========================================\n")
#   cat("        Modeling samples", s.min, "to", s.max, "\n")
#   cat("=========================================\n\n")
  cat(s.min, "to", s.max, ":")

  ts.data <- TSdata(
    output = as.matrix(ts.sub[,labs$bugs]),
    input = as.matrix(ts.sub[,c(labs$imps,labs$news)])
  )
  model <- modeling.methodology(ts.data, verbose = F)
  if(is.null(model)){
    cat("No valid models found for this sample range. Skipping.\n")
    next
  }
  
#   cat("Plotting one-step ahead predictions\n")
  fname <- file.path(out.dir, paste0("one-step_predictions_", s.min, "-", s.max, ".png"))
  plot.predictions(model, s.range, fname, n.plots = 1, width = 1200, height.per = 400, cex = 1.35)

#   cat("=========================================\n")
#   cat("             Forecasting\n")
#   cat("=========================================\n\n")

  tmp1 <- quantile(s$imps[s.range], probs = c(0.25,0.75), type=6)
  tmp2 <- quantile(s$news[s.range], probs = c(0.25,0.75), type=6)

  # Used for hypothetical forecasting. Not differenced values!
  # They will be converted if needed
  imps.hypoth <- seq(from = tmp1[['25%']], to = tmp1[['75%']], by = 2)
  news.hypoth <- seq(from = tmp2[['25%']], to = tmp2[['75%']], by = 1)

  imps.actual <- s$imps[s.max+1]
  news.actual <- s$news[s.max+1]
  bugs.actual <- s$bugs[s.max+1]
  cat("actual imps, news, and bugs:", imps.actual, news.actual, bugs.actual)
  if(!(imps.actual %in% imps.hypoth)){
    imps.hypoth <- sort(append(imps.hypoth, imps.actual))
  }
  if(!(news.actual %in% news.hypoth)){
    news.hypoth <- sort(append(news.hypoth, news.actual))
  }

  if(diffed){
    imps.hypoth <- imps.hypoth - s$imps[s.max]
    news.hypoth <- news.hypoth - s$news[s.max]
  }
  results <- forecast.hypotheticals(model, ts.data, ci=ci,
                         imps.hypoth=imps.hypoth, news.hypoth=news.hypoth)
  x <- results$x; y <- results$y; z <- results$z
  if(diffed){
    x <- x + s$imps[s.max]
    y <- y + s$news[s.max]
    z <- z + s$bugs[s.max]
  }

  actual.at <- which(x == imps.actual & y == news.actual)
  fc.mean <- z[actual.at, ceiling(ncol(z)/2)]
  fc.errs <- append(fc.errs, fc.mean - bugs.actual)

  for(i in 1:floor(ncol(z)/2)){
    ci.lo <- z[actual.at, i]
    ci.hi <- z[actual.at, ncol(z) - i + 1]
    if(bugs.actual >= ci.lo & bugs.actual <= ci.hi){
      ci.inout[i,"in"] <- 1 + ci.inout[i,"in"]
    } else {
      ci.inout[i,"out"] <- 1 + ci.inout[i,"out"]
    }
  }
  
#   library(onion)
#   fname <- file.path(out.dir, paste0("forecast_hypotheticals_mean3d_", s.min, "-", s.max, ".png"))
#   png(filename = fname, width=800, height=800)
#   p3d(x=x,y=y,z=z[,"mean"],d0 = 1,
#       xlab="Improvements", ylab="Features", zlab="Bugs",
#       theta = -120, phi=20, ticktype = "detailed")
#   garbage <- dev.off()
  
  fname <- file.path(out.dir, paste0("forecast_hypotheticals", s.min, "-", s.max, ".csv"))
  forecasts <- data.frame(imps=x, news= y)
  for(cname in colnames(z)){
    forecasts[[cname]] <- z[,cname]
  }
  write.table(forecasts, file = fname, row.names = F, sep = ",")

  cat("\n")
}

for(i in 1:nrow(ci.inout)){
  frac <- ci.inout[i,"in"]/sum(ci.inout[i,])
  cat(paste0(100*round(frac,4),"%"),"in", rownames(ci.inout)[i],"\n")
}
hist(fc.errs, xlab = "Error of forecast mean", main = "", breaks="FD")

