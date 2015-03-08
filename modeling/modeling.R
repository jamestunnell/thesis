est.models <- function(ts.data, max.order){
  cat("Estimating models up to order", max.order)
  models <- list()
  
  for(p in 1:max.order){
    models[[p]] <- estVARXar(ts.data, max.lag = p, aic = F)
    cat(".")
  }
  cat("\n")
  return(models)
}

check.models <- function(models, box.level){
  cat("Checking models for stability and adequacy\n\n")
  
  cat("order\tstable\tBox-Ljung p-val\n")
  unstable.orders <- NULL
  inadequate.orders <- NULL
  not.bad.models <- list()
  
  #   fname <- paste0(OUTDIR, "acf.png")
  #   nrows <- ceiling(p_max/2)
  #   par(mfrow=c(nrows,2))
  #   png(fname, width=1200, height=nrows*200)
  for(p in 1:length(models)){
    model <- models[[p]]
    capture.output(stab <- stability(model))
    residuals <- model$estimates$pred - model$data$output
    
    max.lag = 20
    #     a <- acf(residuals, lag.max = max.lag, plot = F)
    #     plot(a)
    
    lb.test <- Box.test(residuals, type="Ljung-Box", fitdf = p, lag = max.lag)
    cat(p,"\t",stab[[1]],"\t",lb.test$p.value,"\n")
    
    if(stab[[1]] & (lb.test$p.value >= box.level)){
      not.bad.models[[as.character(p)]] <- model
    }
    
    if(!stab[[1]]){
      unstable.orders <- append(unstable.orders, p)
    } 
    
    if(lb.test$p.value < box.level){
      inadequate.orders <- append(inadequate.orders, p)
    }

  }
  #   garbage <- dev.off()
  #   par(mfrow=c(1,1))
  
  if(any(unstable.orders)){
    cat("Unstable model orders:", paste(unstable.orders,collapse = ", "), "\n")
  } else {
    cat("No model orders are unstable\n")
  }
  
  if(any(inadequate.orders)){
    cat("Inadequate model orders:", paste(inadequate.orders,collapse = ", "), "\n")
  } else {
    cat("No model orders are inadequate\n")
  }
  
  return(not.bad.models)
}

modeling.methodology <- function(ts.data){
  
  # Model specification
  K_min = 4
  m <- ncol(ts.data$output) + ncol(ts.data$input)
  n <- nrow(ts.data$output)
  p.max = floor(n/(m*K_min))
  
  # Model estimation
  models <- est.models(ts.data, p.max)
  
  # Model diagnostic checking
  not.bad.models <- check.models(models, box.level = 0.05)
  
  # Model selection
  cat("Selecting best model by AIC\n")
  model <- bestTSestModel(not.bad.models, criterion = "aic")
  cat("Selected model with order", nrow(model$model$A)-1, "\n")
  return(model)
}
