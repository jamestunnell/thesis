'Performs ML estimation for all sub-classes of the Pearson distribution system
via numerical optimization, for model selection purposes. Parameter estimates
for each distribution sub-class are returned. Plots histogram of data, and 
overlays the best estimated distribution.'

library(PearsonDS)

pearson_fns <- c(
  function(x, params){
    return(dpearson0(x, params['mean'], params['sd']))
  },
  function(x, params){
    return(dpearsonI(x, params['a'], params['b'], params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonII(x, params['a'], params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonIII(x, params['shape'], params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonIV(x, params['m'], params['nu'], params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonV(x, params['shape'], params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonVI(x, params['a'],params['b'],  params['location'], params['scale']))
  },
  function(x, params){
    return(dpearsonVII(x, params['df'], params['location'], params['scale'], params['params'], log = FALSE))
  }
)
pearson_fnnames <- c("Pearson Type 0", "Pearson Type I","Pearson Type II","Pearson Type III","Pearson Type IV","Pearson Type V","Pearson Type VI","Pearson Type VII")
pearson_colors <- c("tomato","steelblue3","slategrey","sienna","red","orchid","seagreen","orange")
names(pearson_fnnames) <- 0:7
names(pearson_fns) <- 0:7

fit_subset <- function(subset, type, pri, outdir, image_size){
  W <- image_size[1]
  H <- image_size[2]
  
  fbasename <- paste0(outdir,"/",type,"s_pri",pri,"_fit")
  results <- pearsonMSC(subset)
  cat(capture.output(print(results,quote=F)),file=paste0(fbasename,".txt"),sep="\n")
  
  png(filename = paste0(fbasename,".png"), width=W, height=H)
  title <- paste0("Time to resolve priority ", pri, " ", type, "s(in days)")
  #hist(subset, probability = T,
  plot(density(subset),
       main=title, ylab="Probability",xlab="Time to resolve (days)")
  plotted_types <- NULL
  plotted_colors <- NULL
  
  for(name in names(results$Best)){
    best <- results$Best[[name]]
    type <- as.integer(best[['type']]) + 1
    #cat(paste0("Best type by ", name, ": ", pearson_fnnames[[type]], "\n"))
    cat(".")
    
    if(!(type %in% plotted_types)){
      fn <- pearson_fns[[type]]
      xrange <- range(subset)
      x <- seq(xrange[1],xrange[2],length=1000)
      params <- best
      y <- fn(x,params)
      
      color <- pearson_colors[[type]]
      lines(x,y,col=color)
      plotted_types <- c(plotted_types,type)
      plotted_colors <- c(plotted_colors,color)
    }
  }
  
  n <- length(plotted_types)
  legend("topright", legend = pearson_fnnames[plotted_types], col=plotted_colors, lty=rep(1,n))
  garbage <- dev.off()
}

fit_issues <- function(issue_data, outdir, image_size){  
  for(type in levels(issue_data$type)){
    priorities <- issue_data$priority[issue_data$type == type]    
    for(pri in sort(unique(priorities))){
      indices <- intersect(which(issue_data$type == type),which(issue_data$priority == pri))
      if(length(indices) > 1){
        days <- issue_data$daystoresolve[indices]
        fit_subset(days,type,pri,outdir,image_size)
      }
    }
  }
}