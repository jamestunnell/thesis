#!/usr/bin/env Rscript

'Exploratory data analysis on software issues. Analysis can be on single data set, which
may be from one release period or agreggated from multiple releases. Or, analysis can be
on data from multiple releases, looking for trends across them.

usage:
explore.R INFILE [options]

arguments:
INFILE A text file, containing CSV-like table

options:
--outdir=O  Path to a directory where results can be saved as files [default: ./]
--width=W   Width of output images [default: 800]
--height=H  Height of output images [default: 600]
--log       Send text output to a log file
' -> doc

library(rlist)
library(docopt) # load the docopt library
source("~/projects/thesis/analysis/describe.R")
source("~/projects/thesis/analysis/examine.R")
source("~/projects/thesis/common/util.R")
source("~/projects/thesis/analysis/fit.R")

opts <- docopt(doc) # retrieve the command-line arguments

infile <- opts$INFILE
outdir <- normalizePath(paste0(opts$outdir,"/"))

if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}

W <- as.integer(opts$width)
H <- as.integer(opts$height)

cat(paste0("Reading table from '", infile, "'..."))
data <- readtable(infile)
cat("done.\n")

cat("Describing issue data...")
describe_issues(data,outdir,c(W,H))
cat("done.\n")

cat("Fitting issue data...")
fit_issues(data,outdir,c(W,H))
cat("done.\n")

cat("Examining issue data...")
examine_issues(data,outdir, c(W,H))
cat("done.\n")

# releases <- split_by_release(data)
#   datas <- list.map(infile, x ~ readtable(x))
#   cat("done.\n")
#   
#   cat("Describing issue datas...")
#   describe_issues_multi(datas,outdir,c(W,H))
#   cat("done.\n")
#   
#   cat("Fitting issue datas...")
#   #fit_issues_multi(datas, outdir, c(W,H))
#   cat("done.\n")
# }

if(opts$log){
  warnings()
}
