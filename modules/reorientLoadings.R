reorientLoadings = function (loadings, pivot) {
  itemsCount = dim(loadings)[1]
  factorsCount = dim(loadings)[2]
  signReflectionsCount = 2 ^ factorsCount
  columnPermutationsCount = factorial(factorsCount)
  orientationsCount = signReflectionsCount * columnPermutationsCount
  signReflections = gtools::permutations(2, factorsCount, c(1, -1), repeats.allowed = TRUE)
  columnPermutations = gtools::permutations(factorsCount, factorsCount)
  distances = list()
  orientations1 = list()
  orientations2 = list()
  orientations3 = list()
  
  for (j in 1:signReflectionsCount) {
    for (k in 1:columnPermutationsCount) {
      index = (j - 1) * columnPermutationsCount + k
      
      orientations1[[index]] = swapColumns(loadings[,, 1] * matrix(signReflections[j,], nrow = itemsCount, ncol = factorsCount, byrow = TRUE), columnPermutations[k,])
      orientations2[[index]] = swapColumns(loadings[,, 2] * matrix(signReflections[j,], nrow = itemsCount, ncol = factorsCount, byrow = TRUE), columnPermutations[k,])
      orientations3[[index]] = swapColumns(loadings[,, 3] * matrix(signReflections[j,], nrow = itemsCount, ncol = factorsCount, byrow = TRUE), columnPermutations[k,])
      distances[[index]] = euclideanDistance(as.vector(pivot), as.vector(orientations1[[index]]))
    }
  }
  
  result = array(0, dim(loadings))
  
  result[,, 1] = orientations1[[which.min(unlist(distances))]]
  result[,, 2] = orientations2[[which.min(unlist(distances))]]
  result[,, 3] = orientations3[[which.min(unlist(distances))]]
  
  for (i in 1:itemsCount) {
    for (j in 1:factorsCount) {
      lowerLoading = min(result[i, j, 2], result[i, j, 3])
      higherLoading = max(result[i, j, 2], result[i, j, 3])
      
      result[i, j, 2] = lowerLoading
      result[i, j, 3] = higherLoading
    }
  }
  
  return (result)
}
