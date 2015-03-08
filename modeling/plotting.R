plot.tseries <- function(tseries, fname, diff){
  png(fname, width=1200, height=1000)
  par(mfrow=c(3,1),cex=1.5)
  
  n <- length(tseries[[1]])
  for(i in 1:length(tseries)){
    x <- if(any(diff)){ 2:(n+1) } else { 1:n }
    y <- tseries[[i]]
    name <- names(tseries)[i]
    plot(x, y, type="l",
         main = if(diff[i]){ paste(name,"(differenced)") } else { name },
         xlab = "time sample",
         ylab = if(diff[i]){ "count difference" } else { "count" })
  }
  dev.off()
  par(mfrow=c(1,1),cex=1)  
}

plot.predictions <- function(model, fname, diff, n.plots = 1, width = 1200, height.per = 300, cex = 1.5){
  y <- model$data$output
  n <- length(y)
  x <- if(diff){ 2:(n+1) } else { 1:n }
  
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
    
    plot(x[l:r],y[l:r],main="", type="l",
         xlab="time sample",
         ylab=paste("count", if(diff){ "difference" }))
    lines(x[l:r],model$estimates$pred[l:r], col="red", lty=2)
    legend("topleft",legend=c("Actual","Fitted"),col=c("black","red"), lty=c(1,2))
    l <- r + 1
  }
  garbage <- dev.off()
  par(mfrow=c(1,1), cex=1)
}