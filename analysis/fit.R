'Performs ML estimation for all sub-classes of the Pearson distribution system
via numerical optimization, for model selection purposes. Parameter estimates
for each distribution sub-class are returned. Plots histogram of data, and 
overlays the best estimated distribution.'

library(PearsonDS)
library(hash)
source("~/projects/thesis/common/pearson.R")

fit_issues <- function(issue_data, outdir, image_size, pearson_type = NULL){  
  W <- image_size[1]
  H <- image_size[2]
  for(typ in c("bug","improvement","newfeature","task")){
    priorities <- issue_data$priority[issue_data$type == typ]    
    for(pri in sort(unique(priorities))){
      indices <- intersect(which(issue_data$type == typ),which(issue_data$priority == pri))
      if(length(indices) > 1){
        days <- issue_data$daystoresolve[indices]
        write.table(days, file = paste0(outdir,"/",typ,"s_pri",pri,".txt"),
                    row.names=F, col.names=F)
        n <- length(days)
        if(n < 30){
          days <- append(days, sample(days, 30 - n, replace=TRUE))
        }
        results <- pearson_model(days,pearson_type)
        
        fbasename <- paste0(outdir,"/fit_",typ,"s_pri",pri)
        cat(capture.output(print(results$results,quote=F)),file=paste0(fbasename,".txt"),sep="\n")
        png(filename = paste0(fbasename,".png"), width=W, height=H)
        title <- paste0("Time to resolve priority ", pri, " ", typ, "s(in days)")
        hist(days, probability = T, breaks="FD",
             main=title, ylab="Probability",xlab="Time to resolve (days)")

        fn <- results$dfunc
        xrange <- range(days)
        x <- seq(xrange[1],xrange[2],length=1000)
        params <- results$params
        y <- fn(x,params)
        lines(x,y,col='red')
        name <- paste("Pearson Type",results$type)
        legend("topright", legend=c(name), col=c('red'), lty=c(1))
        garbage <- dev.off()
      }
    }
  }
}

# fit_issues_multi <- function(issue_datasets, outdir, image_size){
#   #best_pearson_types = hash()
#   as <- hash()
#   bs <- hash()
#   locs <- hash()
#   scales <- hash()
#   keys <- NULL
#   for(ds in issue_datasets){
#     for(type in c("bug","newfeature","improvement")){
#       priorities <- ds$priority[ds$type == type]
#       #for(pri in sort(unique(priorities))){
#       #  indices <- intersect(which(ds$type == type),which(ds$priority == pri))
#       #  if(length(indices) > 1){
#       #    key = paste0(type,pri)
#       #    keys <- append(keys,key)
#       #    
#       #    days <- ds$daystoresolve[indices]
#           key = type
#           keys <- append(keys,key)
#           days <- ds$daystoresolve[ds$type == type]
#       
#           results <- pearsonMSC(days)
#           bestML <- results$Best$ML
#           
#           as[[key]] <- append(as[[key]], results$FittedDistributions$I[['a']])
#           bs[[key]] <- append(bs[[key]], results$FittedDistributions$I[['b']])
#           locs[[key]] <- append(locs[[key]], results$FittedDistributions$I[['location']])
#           scales[[key]] <- append(scales[[key]], results$FittedDistributions$I[['scale']])
#           
#           #pearson_params[[key]] <- c(pearson_params[[key]],results$FittedDistributions$I)
#           #best_pearson_types[[key]] <- c(best_pearson_types[[key]],bestML[['type']])
#       #  }
#       #}
#     }
#   }
# 
#   for(key in unique(keys)){
#     cat("Key: ", key, "\n")
#     #cat(paste0("shapes: ", shapes[[key]],"\n"))
#     #cat(paste0("locs: ", locs[[key]],"\n"))
#     #cat(paste0("scales: ", scales[[key]],"\n"))
#     #freqcount <- table(best_pearson_types[[key]])
#     png(filename = paste0(outdir,"/",key,"_multi_fit.png"), width=W, height=H)
#     #barplot(freqcount, xlab="Pearson type", ylab="Count")
#     par(mfrow=c(1,4))
#     hist(as[[key]],main="a")
#     hist(bs[[key]],main="b")
#     hist(locs[[key]],main="location")
#     hist(scales[[key]],main="scale")
#     garbage <- dev.off()
#     par(mfrow=c(1,1))
#   }
# }
