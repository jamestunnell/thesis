#!/usr/bin/env Rscript


'Forecast software defects using time series model of historical data.

usage:
sample.R ISSUES_FILE SAMPLE_FILE [options]

arguments:
ISSUES_FILE   A text file, containing CSV-like table, with bug data from all releases.
SAMPLE_FILE   File to output sampling results to.

options:
--period=P  Sample period (in days) [default: 7]
' -> doc

library(docopt) # load the docopt library
opts <- docopt(doc) # retrieve the command-line arguments
# opts <- list(
#   ISSUES_FILE = "~/projects/thesis/data/mongodb/issues.txt",
#   SAMPLE_FILE = "~/projects/thesis/modeling/mongodb/samples.txt"
# )
issues_file <- opts$ISSUES_FILE
sample_file <- opts$SAMPLE_FILE

source("~/projects/thesis/modeling/sampling.R")

issues_file <- opts$ISSUES_FILE
issues <- read.table(issues_file, header = TRUE)
period <- as.numeric(opts$period)

issues$created <- timeDate(issues$created)
issues$resolved <- timeDate(issues$resolved)
date.min <- min(issues$created,issues$resolved)
date.max <- max(issues$created,issues$resolved)

s <- sample.dateRange(issues,date.min,date.max,period)
write.table(s,file = sample_file, row.names = F)