extractSamples <- function (fit, parameters = NULL) {
  samples <- list()
  
  if (!is.vector(parameters)) {
    parameters <- fit@model_pars;
  }
  
  for (i in 1:(length(parameters))) {
    parameter <- rstan::extract(fit, parameters[i])[[1]]
    
    if (length(dim(parameter)) > 1) {
      parameter <- aperm(parameter, c(2:length(dim(parameter)), 1))
    }
    
    samples[[fit@model_pars[i]]] <- parameter;
  }
  
  return (samples)
}
