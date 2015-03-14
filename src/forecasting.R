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

# forecast.hypotheticals.conf2d <- function(
#   model.est, data.base, imps.hypoth, news.hypoth, fname){
#   
#   data.ext <- ts.extend.one(data.base)
#   row.last <- nrow(data.ext$input)
#   labs <- ts.labels(data.base)
#   
# #   png(filename = fname, width=800, height=800)
#   for(nr in news.hypoth){
#     x <- NULL; y <- NULL; ylo <- NULL; yhi <- NULL;
#     for(ir in imps.hypoth){
#       data.ext$input[row.last,c(labs$imps,labs$news)] <- c(ir,nr)
#       fc <- forecast(TSmodel(model.est), data.ext)
# #       x <- append(x,ir)
# #       y <- append(y,)
# #       z <- append(z, fc$forecast[[1]][1,])
#     }
#   }
# }