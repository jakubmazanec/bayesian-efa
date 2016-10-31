reorientSamples = function (samples, pivot, factorsCount) {
  itemsCount = dim(samples)[1]
  signReflectionsCount = 2 ^ factorsCount
  columnPermutationsCount = factorial(factorsCount)
  orientationsCount = signReflectionsCount * columnPermutationsCount
  signReflections = gtools::permutations(2, factorsCount, c(1, -1), repeats.allowed = TRUE)
  columnPermutations = gtools::permutations(factorsCount, factorsCount)
  distances = list()
  orientations = list()
  
  for (i in 1:(dim(samples)[3])) {
    for (j in 1:signReflectionsCount) {
      for (k in 1:columnPermutationsCount) {
        index = (j - 1) * columnPermutationsCount + k
        
        orientations[[index]] = swapColumns(samples[,, i] * matrix(signReflections[j,], nrow = itemsCount, ncol = factorsCount, byrow = TRUE), columnPermutations[k,])
        distances[[index]] = euclideanDistance(as.vector(pivot), as.vector(orientations[[index]]))
      }
    }
    
    samples[,, i] = orientations[[which.min(unlist(distances))]]
  }
  
  return (samples)
}

befa = function(data, factorsCount) {
  scaledResponses = scale(data)
  itemsCount = dim(data)[2]
  subjectsCount = dim(data)[1]
  startTime = proc.time()
  
  getInitValues = function() {
    values = list(factorLoadings = matrix(0, nrow = itemsCount, ncol = factorsCount) + runif(itemsCount * factorsCount, -1, 1),
                  itemVariances = rep(0.5, itemsCount) + runif(itemsCount, -0.25, 0.25),
                  itemVariancesMean = 0.25 + runif(1, -0.1, 0.1),
                  itemVariancesSd = 0.15 + runif(1, -0.1, 0.1))
    
    return (values);
  }
  
  cat("Bayesian EFA...\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")
  
  fit = stan("befa.stan",
             data = list("responses" = scaledResponses,
                         "subjectsCount" = subjectsCount,
                         "itemsCount" = itemsCount,
                         "factorsCount" = factorsCount),
             warmup = 150,
             iter = 750,
             init = getInitValues,
             chains = 1)
  samples = extractSamples(fit)
  rotatedLoadingsSamples = array(0, dim(samples$factorLoadings))
  
  for (i in 1:(dim(samples$factorLoadings)[3])) {
    rotatedLoadingsSamples[,,i] = varimax(samples$factorLoadings[,,i], normalize = FALSE)$loadings
  }
  
  cat("\nReorienting samples...\n")
  
  progressBar = txtProgressBar(min = 0, max = 20, style = 3)
  reorientedLoadingsSamples = rotatedLoadingsSamples
  reorientedLoadingsSamples = reorientSamples(reorientedLoadingsSamples, reorientedLoadingsSamples[,, 1], factorsCount)
  
  setTxtProgressBar(progressBar, 1)
  
  # iterate reorientation procedure until convergence
  for (iteration in 1:19) {
    pivot = array(0, c(itemsCount, factorsCount))
    
    for (i in 1:itemsCount) {
      for (j in 1:factorsCount) {
        pivot[i, j] = mean(reorientedLoadingsSamples[i, j,])
      }
    }
    
    reorientedLoadingsSamples = reorientSamples(reorientedLoadingsSamples, pivot, factorsCount)
    nextPivot = array(0, c(itemsCount, factorsCount))
    
    for (i in 1:itemsCount) {
      for (j in 1:factorsCount) {
        nextPivot[i, j] = mean(reorientedLoadingsSamples[i, j,])
      }
    }
    
    meanDelta = sqrt((nextPivot - pivot) ^ 2)
    
    setTxtProgressBar(progressBar, iteration + 1)
    
    if (mean(meanDelta) <= 0.001) {
      break;
    }
  }
  
  setTxtProgressBar(progressBar, 20)
  
  cat("\n")
  
  factorLoadings = array(0, c(itemsCount, factorsCount, 3))
  
  for (i in 1:itemsCount) {
    for (j in 1:factorsCount) {
      factorLoadings[i, j,] = summarizeSamples(reorientedLoadingsSamples[i, j,])[c("mean", "hdiLower", "hdiUpper")]
    }
  }
  
  cat("\nDone!\n")
  
  endTime = proc.time()
  duration = (endTime - startTime)[3]
  
  cat("Elapsed time:", duration, "s\n")
  
  return (list("fit" = fit,
               "factorLoadings" = factorLoadings,
               "duration" = duration))
}
