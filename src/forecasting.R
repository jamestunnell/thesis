library(dse)

ts.labels <- function(ts.data){
  names.out <- colnames(ts.data$output)
  names.in <- colnames(ts.data$input)
  labs <- list(bugs = names.out[pmatch("Bug",names.out)],
               imps = names.in[pmatch("Imp",names.in)],
               news = names.in[pmatch("Fea",names.in)])
  return(labs)
}

ts.extend.one <- function(ts.data){
  data.ext <- TSdata(
    output = ts.data$output,
    input = rbind(ts.data$input, matrix(c(0,0),nrow=1))
  )
  return(data.ext)
}

forecast.hypotheticals.mean3d <- function(
  model.est, data.base, imps.hypoth, news.hypoth, fname){
  
  data.ext <- ts.extend.one(data.base)
  row.last <- nrow(data.ext$input)
  labs <- ts.labels(data.base)
  
  x <- NULL; y <- NULL; z <- NULL;
  for(ir in imps.hypoth){
    for(nr in news.hypoth){
      data.ext$input[row.last,c(labs$imps,labs$news)] <- c(ir,nr)
      fc <- forecast(TSmodel(model.est), data.ext)
      x <- append(x,ir)
      y <- append(y,nr)
      z <- append(z, fc$forecast[[1]][1,])
    }
  }
  library(onion)
  png(filename = fname, width=800, height=800)
  p3d(x=x,y=y,z=z,d0 = 1,
      xlab=labs$imps, ylab=labs$news, zlab=labs$bugs,
      theta = -60, phi=30, ticktype = "detailed")
  garbage <- dev.off()
  return(list(x=x,y=y,z=z))
}

forecast.intervals <- function(model, ci){
  alpha <- 1 - ci
  z <- qnorm(1-alpha/2)
  residuals <- (model$estimates$pred - model$data$output)
  mse <- sum(residuals)^2 / length(residuals)
  return(c(-z,z)*sqrt(mse))
}

# Plot forecast (w/ confidence intervals) for different
# hypothetical  values of nimps and nnews. Two side-by-side
# plots are produced, one using hypothetical nimps and actual
# nnews, and vice-versa.
forecast.hypotheticals.conf2d <- function(
  model.est, data.base, ci, fname,
  imps.actual, news.actual, imps.hypoth, news.hypoth){
  
  data.ext <- ts.extend.one(data.base)
  row.last <- nrow(data.ext$input)
  labs <- ts.labels(data.base)
  
  d <- forecast.intervals(model, ci)
  
  nnews <- length(news.hypoth)
  nimps <- length(imps.hypoth)
  
  png(filename = fname, width=800, height=400)
  par(mfrow=c(1,2))
  
  x <- imps.hypoth
  y <- mat.or.vec(nimps,3)
  for(i in 1:nimps){
    data.ext$input[row.last,c(labs$imps,labs$news)] <- c(x[i],news.actual)
    fc <- forecast(TSmodel(model.est), data.ext)
    y_ <- fc$forecast[[1]][1,]
    lohi <- y_ + d
    y[i,1] <- lohi[1]
    y[i,2] <- y_
    y[i,3] <- lohi[2]
  }
  plot(x,y[,2], type="l", ylim=range(y), xlab=labs$imps, ylab=labs$bugs,
       main=paste(labs$news,"(actual) =",news.actual))
  lines(x,y[,1], lty=2)
  lines(x,y[,3], lty=2)
  
  x <- news.hypoth
  y <- mat.or.vec(nnews,3)
  for(i in 1:nnews){
    data.ext$input[row.last,c(labs$imps,labs$news)] <- rev(c(x[i],imps.actual))
    fc <- forecast(TSmodel(model.est), data.ext)
    y_ <- fc$forecast[[1]][1,]
    lohi <- y_ + d
    y[i,1] <- lohi[1]
    y[i,2] <- y_
    y[i,3] <- lohi[2]
  }
  plot(x,y[,2], type="l", ylim=range(y), xlab=labs$news, ylab=labs$bugs,
       main=paste(labs$imps,"(actual) =",imps.actual))
  lines(x,y[,1], lty=2)
  lines(x,y[,3], lty=2)
  
  garbage <- dev.off()
  par(mfrow=c(1,1))
}