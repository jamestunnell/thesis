#!/usr/bin/env Rscript

'Separates single table data, by categories, into separate tables.

usage:
separate_data.R INFILE OUTDIR [options]

arguments:
INFILE A text file, containing CSV-like table
OUTDIR Path to a directory where separate table filescan be saved

options:
--log       Send text output to a log file
' -> doc

library(docopt) # load the docopt library

opts <- docopt(doc) # retrieve the command-line arguments

infile <- opts$INFILE
outdir <- normalizePath(paste0(opts$OUTDIR,"/"))

if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}

cat(c("Reading table from", infile, "...\n"))

data <- read.table(infile, header = TRUE, colClasses = c("factor","factor","numeric"))
attach(data)

for(typ in levels(type)){
  for(pri in levels(priority)){
    indices <- intersect(which(type == typ),which(priority == pri))
    if(length(indices) > 0){
      days = daystoresolve[indices]
      write(days, file=paste0(typ,"s_pri",pri,".txt"), ncolumns=1)  
    }
  }
}