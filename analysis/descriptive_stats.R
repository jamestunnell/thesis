#!/usr/bin/env Rscript

'usage:
    descriptive_stats.R INFILE OUTDIR [options]

arguments:
    INFILE A text file, containing CSV-like table
    OUTDIR Path to a directory where descriptive statistics results can be saved as files

options:
  -l --log  Send text output to a log file
' -> doc

library(docopt) # load the docopt library

opts <- docopt(doc) # retrieve the command-line arguments

infile <- opts$INFILE
outdir <- normalizePath(paste0(opts$OUTDIR,"/"))

if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}

cat(c("Reading table from", infile, "...\n"))

data <- read.table(infile, header = TRUE)
attach(data)

# -----------------------------------------------------------------------

cat(c("Beginning analyis, writing results to",outdir,"...\n"))

summary_file <- paste0(outdir,"/summary.txt")
cat("",file=summary_file)
for(typ in levels(type)){
  priorities <- priority[type == typ]
  freqcounts <- table(priorities)
  fbasename <- paste0(outdir,"/",typ,"_priority_freqcount")

#    pngname <- paste0(outdir,"/",typ,"_daystoresolve_boxplot.png")
#    png(filename = pngname, width=800, height=800)
#    boxplot(priority[type==typ],daystoresolve[type==typ],outline=FALSE)
#    garbage <- dev.off()
  
#   ## put freq counts in text table
#   #write.table(freqcounts, paste0(fbasename,".txt"), sep=" ")
#   
  # put freq counts in bar plot image
  png(filename = paste0(fbasename,".png"), width=800, height=800)
  title <- paste0("Frequency of ", typ, "s, by priority")
  barplot(freqcounts,main=title,xlab="priority")
  dev.off()
   
  # histogram of days-to-resolve by priority
  for(pri in sort(unique(priorities))){
    indices <- intersect(which(type == typ),which(priority == pri))
    if(length(indices) > 0){
      days = daystoresolve[indices]
      title <- paste0("Time to resolve priority ", pri, " ", typ, "s (in days)")
      xlab <- "Time to resolve (days)"
      
      # summary stats
      cat(title,file=summary_file,sep="\n",append=TRUE)
      out<-capture.output(summary(days))
      cat(out,file=summary_file,sep="\n",append=TRUE)
      cat("",file=summary_file,sep="\n",append=TRUE)
      
      # boxplot and histogram, side-by-side
      pngname <- paste0(outdir,"/",typ,"_pri",pri,"_daystoresolve.png")
      png(filename = pngname, width=800, height=800)
      par(mfrow=c(2,1)) # make sure this is placed AFTER calling the device (e.g., png())
      boxplot(days,outline=FALSE, horizontal = T, main=title, xlab=xlab)
      hist(days, breaks="FD", probability = F, main=title, xlab=xlab)
      dev.off()
      par(mfrow=c(1,1))
    }
  }
}

cat("Analysis complete.\n")
