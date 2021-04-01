########################################################
## Create Zelig-style (TomzKing style) simulation data
########################################################
getSimData <- function(newData, fit, label, nSims=1000) {
  simDraws <- mvrnorm(nSims, coef(fit), vcov(fit))
  simYhats <- simDraws[,c( "(Intercept)", "xZ")] %*% t(newData)
  mu <- coef(fit)['(Intercept)']
  
  # Combine scenario definitions and predictions.
  out <- data.frame( Improvement=(as.numeric(simYhats) / mu) - 1.0, Label=label, stringsAsFactors=FALSE )
  return(out)
}


