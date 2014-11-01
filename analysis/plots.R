#!/usr/bin/env Rscript

'usage:
    plots.R INFILE OUTDIR [options]

arguments:
    INFILE A text file, containing CSV-like table
    OUTDIR Path to a directory where histograms can be saved as files

options:
  -l --log  Send text output to a log file
' -> doc

library(docopt) # load the docopt library
opts <- docopt(doc) # retrieve the command-line arguments

infile <- opts$INFILE
outdir <- opts$OUTDIR

if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}

cat(c("Reading table from", infile, "...\n"))

data <- read.table(infile, header = TRUE)
attach(data)


cat(c("Writing plots to",outdir,"...\n"))

for(type_level in levels(type)){
  pngname <- normalizePath(paste0(outdir,"/",type_level,"_barplot.png"))
  png(filename = pngname, width=800, height=800)
  barplot(table(priority[type == type_level]),
          main=paste0("frequency of ", type_level, "s, by priority"),
          xlab="priority")
  dev.off()
  
  pngname <- normalizePath(paste0(outdir,"/",type_level,"_hist.png"))
  png(filename = pngname, width=800, height=800)
  hist(daystoresolve[type == type_level], freq=FALSE, breaks = "FD",
       main=paste0("Histogram of time to resolve ", type_level, "s, in days"),
       xlab="Time to resolve (days)")
  dev.off()
}

pngname <- normalizePath(paste0(outdir,"/box_plots.png"))
png(filename = pngname, width=800, height=800)
plot(type,daystoresolve)
garbage <- dev.off()

cat("Plotting complete.")