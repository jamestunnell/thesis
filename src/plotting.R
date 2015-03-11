ts.plot <- function(ts, fname, width = 800, height.each = 200, cex=1.5){
  png(fname, width=width, height=height.each*ncol(ts))
  par(cex.lab=cex, cex.axis=cex, mfrow=c(ncol(ts),1))
  
  for(i in 1:ncol(ts)){
    y <- as.vector(ts[,i])
    n <- length(y)
    plot(time(ts), y, main=NULL, type="l", xlab = "time", ylab = names(ts)[i])
  }
  
  dev.off()
  par(cex.lab=1, cex.axis=1, mfrow=c(1,1))
}

plot.predictions <- function(model, x.range, fname, n.plots = 1, width = 800, height.per = 200, cex = 1.5){
  y <- model$data$output
  n <- length(y)
  x <- x.range
  
  png(fname, width=width, height=n.plots*height.per)
  par(mfrow=c(n.plots,1), cex=cex)
  f = n %% n.plots
  l <- 1
  m <- ceiling(n/n.plots)
  for(i in 1:n.plots){
    if(i <= f){
      r <- l + m - 1
    } else {
      r <- l + m - 2
    }
    
    plot(x[l:r], y[l:r],main=NULL, type="l", xlab="week", ylab=colnames(y))
    lines(x[l:r],model$estimates$pred[l:r], col="red", lty=2)
    legend("topleft",legend=c("Actual","Fitted"),col=c("black","red"), lty=c(1,2))
    l <- r + 1
  }
  garbage <- dev.off()
  par(mfrow=c(1,1), cex=1)
}