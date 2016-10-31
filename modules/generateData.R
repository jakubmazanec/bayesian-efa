generateData = function (subjectsCount, factorLoadings, interceptsMean = 0, interceptsSd = 0, factorScoresSd = 1, errorSdsMax = 1, breaks = FALSE, seed = 42) {
  stopifnot(is.numeric(subjectsCount))
  stopifnot(is.matrix(factorLoadings))
  stopifnot(is.numeric(interceptsMean))
  stopifnot(is.numeric(interceptsSd))
  stopifnot(is.numeric(factorScoresSd))
  stopifnot(is.numeric(errorSdsMax))
  
  set.seed(seed)
  
  itemsCount = dim(factorLoadings)[1]
  factorsCount = dim(factorLoadings)[2]
  responses = array(0, dim = c(subjectsCount, itemsCount))
  communalities = array(0, dim = c(subjectsCount, itemsCount))
  errors = array(0, dim = c(subjectsCount, itemsCount))
  errorRatios = array(0, dim = c(subjectsCount, itemsCount))
  intercepts = zeros(itemsCount)
  errorSds = zeros(itemsCount)
  factorScores = array(0, dim = c(subjectsCount, factorsCount))
  
  for (i in 1:subjectsCount) {
    for (j in 1:factorsCount) {
      factorScores[i, j] = rnorm(1, 0, factorScoresSd)
    }
  }
  
  for (i in 1:itemsCount) {
    intercepts[i] = rnorm(1, interceptsMean, interceptsSd)
  }
  
  for (i in 1:itemsCount) {
    errorSds[i] = runif(1, 0, errorSdsMax)
  }
  
  for (i in 1:subjectsCount) {
    for (j in 1:itemsCount) {
      errors[i, j] = rnorm(1, 0, errorSds[j])
      communalities[i, j] = 0
      errorRatios[i, j] = 0
      
      for (k in 1:factorsCount) {
        communalities[i, j] = communalities[i, j] + factorLoadings[j, k] * factorScores[i, k]
        errorRatios[i, j] = errorRatios[i, j] + (factorLoadings[j, k] * factorScores[i, k]) ^ 2
      }
      
      responses[i, j] = communalities[i, j] + intercepts[j] + errors[i, j]
      errorRatios[i, j] = sqrt((errors[i, j] ^ 2) / errorRatios[i, j])
      
      if (is.vector(breaks) && breaks) {
        responses[i, j] = as.integer(cut(c(responses[i, j]), breaks=c(-Inf, breaks, Inf)))
      }
    }
  }
  
  return (list("subjectsCount" = subjectsCount,
               "itemsCount" = itemsCount,
               "factorsCount" = factorsCount,
               "responses" = responses,
               "communalities" = communalities,
               "errors" = errors,
               "errorRatios" = errorRatios,
               "factorLoadings" = factorLoadings,
               "factorScores" = factorScores,
               "intercepts" = intercepts,
               "errorSds" = errorSds))
}
