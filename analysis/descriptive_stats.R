#!/usr/bin/env Rscript

'usage:
    descriptive_stats.R INFILE OUTDIR [options]

arguments:
    INFILE A text file, containing CSV-like table
    OUTDIR Path to a directory where descriptive statistics results can be saved as files

options:
  --width=W   Width of output images [default: 800]
  --height=H  Height of output images [default: 600]
  --combine   Put barplot and boxplot in single image
  --log       Send text output to a log file
  --flip      Flip orientation of plots
' -> doc

library(docopt) # load the docopt library

opts <- docopt(doc) # retrieve the command-line arguments

infile <- opts$INFILE
outdir <- normalizePath(paste0(opts$OUTDIR,"/"))

if(opts$log){
  sink(file = paste0(outdir,"/log.txt"))
}

W <- as.numeric(opts$width)
H <- as.numeric(opts$height)

cat(c("Reading table from", infile, "...\n"))

data <- read.table(infile, header = TRUE, colClasses = c("factor","factor","numeric"))
attach(data)

# -----------------------------------------------------------------------

cat(c("Beginning analyis, writing results to",outdir,"...\n"))

plot_freqcounts <- function(freqcounts, title, horizontal){
  xlab <- "Priority"; ylab <- "Count"
  if(horizontal){ z <- xlab; xlab <- ylab; ylab <- z }
  barplot(freqcounts, horiz = horizontal,
          main=title,xlab=xlab, ylab=ylab)
}

plot_daystoresolve <- function(days,priorities,title, horizontal){
  xlab <- "Priority"; ylab <- "Time to resolve (days)"
  if(horizontal){ z <- xlab; xlab <- ylab; ylab <- z }
  boxplot(days~priorities, outline = F, horizontal = horizontal,
          main=title, xlab=xlab, ylab=ylab)
}

summary_file <- paste0(outdir,"/summary.txt")
cat("",file=summary_file)
for(typ in levels(type)){
  priorities <- priority[type == typ]
  days <- daystoresolve[type == typ]
  freqcounts <- table(priorities)
  fbasename <- paste0(outdir,"/",typ,"s")
  
  bartitle <- paste0("Frequency of ", typ, "s, by priority")
  boxtitle <- paste0("Time to resolve ", typ, "s (in days)")
  
  # write frequency counts as table in text file
  cat(bartitle,file=summary_file,sep="\n",append=TRUE)
  out<-capture.output(table(priorities))
  cat(out,file=summary_file,sep="\n",append=TRUE)
  cat("",file=summary_file,sep="\n",append=TRUE)
  
  hor <- opts$flip
  if(opts$combine){
    png(filename = paste0(fbasename,".png"), width=W, height=H)
    # make sure this is placed AFTER calling the device (e.g., png())
    if(opts$flip){
      par(mfrow=c(2,1))
    }
    else{
      par(mfrow=c(1,2))
    }
    plot_freqcounts(freqcounts,bartitle,opts$flip)
    plot_daystoresolve(days,priorities,boxtitle,opts$flip)
    par(mfrow=c(1,1))
  }
  else {
    png(filename = paste0(fbasename,"_barplot.png"), width=W, height=H)
    plot_freqcounts(freqcounts,bartitle,opts$flip)
    dev.off()
    
    png(filename = paste0(fbasename,"_boxplot.png"), width=W, height=H)
    plot_daystoresolve(days,priorities,boxtitle,opts$flip)
    dev.off()
  }
  
#    pngname <- paste0(outdir,"/",typ,"_daystoresolve_boxplot.png")
#    png(filename = pngname, width=800, height=800)
#    boxplot(priority[type==typ],daystoresolve[type==typ],outline=FALSE)
#    garbage <- dev.off()
  
#   ## put freq counts in text table
#   #write.table(freqcounts, paste0(fbasename,".txt"), sep=" ")
#   

   # histogram of days-to-resolve by priority
   for(pri in sort(unique(priorities))){
     indices <- intersect(which(type == typ),which(priority == pri))
     if(length(indices) > 0){
       days <- daystoresolve[indices]
       title <- paste0("Time to resolve priority ", pri, " ", typ, "s (in days)")
       
       # summary stats
       cat(title,file=summary_file,sep="\n",append=TRUE)
       out<-capture.output(summary(days))
       cat(out,file=summary_file,sep="\n",append=TRUE)
       cat("",file=summary_file,sep="\n",append=TRUE)
#
#       xlab <- "Time to resolve (days)"
#
#       # boxplot and histogram, side-by-side
#       pngname <- paste0(outdir,"/",typ,"_pri",pri,".png")
#       png(filename = pngname, width=800, height=800)
#       #par(mfrow=c(2,1)) # make sure this is placed AFTER calling the device (e.g., png())
#       boxplot(days,outline=FALSE, horizontal = T, main=title, xlab=xlab)
#       #hist(days, breaks="FD", probability = F, main=title, xlab=xlab)
#       dev.off()
#       #par(mfrow=c(1,1))
     }
   }
}

cat("Analysis complete.\n")
