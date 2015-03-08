THESIS_DIR <- "~/projects/thesis/"
ISSUES_FILE = paste0(THESIS_DIR,"data/mongodb/issues.txt")
OUTDIR = paste0(THESIS_DIR,"proposal/images/")
PERIOD = 7
ST_TYPE = "constant"

require(dse)
source(paste0(THESIS_DIR,"modeling/sampling.R"))
source(paste0(THESIS_DIR,"modeling/testing.R"))
source(paste0(THESIS_DIR,"modeling/modeling.R"))
source(paste0(THESIS_DIR,"modeling/plotting.R"))

issues <- read.table(ISSUES_FILE, header = T)
s <- sample.issues.all(issues, PERIOD)

bc <- s$bugs.created
ir <- s$imps.resolved
nr <- s$news.resolved

tseries = list("Bugs Created"=bc, "Improvements Resolved"=ir, "New Features Resolved"=nr)
diff = c(F,F,F)

cat("=========================================\n")
cat("             Pre-Modeling\n")
cat("=========================================\n\n")

cat("Plotting time-series\n")
fname <- paste0(OUTDIR, "time_series.png")
plot.tseries(tseries, fname, diff = diff)

fname <- paste0(OUTDIR, "stationarity.txt")
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
  fname <- paste0(OUTDIR, "time_series_diff.png")
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
  fname <- paste0(OUTDIR, s.min, "-", s.max, "_one-step_predictions.png")
  plot.predictions(model, fname, diff[1], n.plots = 1, width = 1200, height.per = 400, cex = 1.35)
}