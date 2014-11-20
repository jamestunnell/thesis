describe_issues <- function(issue_data, outdir, image_size){
  W <- image_size[1]
  H <- image_size[2]
  summary_file <- paste0(outdir,"/summary.txt")
  cat("",file=summary_file)
  for(type in levels(issue_data$type)){
    priorities <- issue_data$priority[issue_data$type == type]    
    freqcounts <- table(priorities)

    png(filename = paste0(outdir,"/",type,"s.png"), width=W, height=H)
    par(mfrow=c(1,2))
    
    xlab <- "Priority"; ylab <- "Count"
    title <- paste0("Frequency of ", type, "s, by priority")
    barplot(freqcounts, main=title,xlab=xlab, ylab=ylab)
    
    out<-capture.output(freqcounts)
    cat(c(title,out,""),file=summary_file,sep="\n",append=TRUE)  
    
    title <- paste0("Time to resolve ", type, "s (in days)")
    days <- issue_data$daystoresolve[issue_data$type == type]
    boxplot(days~priorities, outline = F, main=title, xlab=xlab, ylab="Time to resolve (days)")
    
    dev.off()
    par(mfrow=c(1,1))

    # summary stats of days-to-resolve, by priority
    for(pri in sort(unique(priorities))){
      indices <- intersect(which(issue_data$type == type),which(issue_data$priority == pri))
      if(length(indices) > 0){
        days <- issue_data$daystoresolve[indices]
        title <- paste0("Time to resolve priority ", pri, " ", type, "s (in days)")

        out<-capture.output(summary(days))
        cat(c(title,out,""),file=summary_file,sep="\n",append=TRUE)
      }
    }
  }
}