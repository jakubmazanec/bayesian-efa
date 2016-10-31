# generates bootstrapped factor loadings for CEFA
bootstrapFactorLoadings = function(data, factorsCount, itemsCount, iterationsCount = 1000) {
  originalLoadings = fa(data,
                        nfactors = factorsCount,
                        rotate = "varimax",
                        fm = "ml")$loadings
  bootstrappedLoadings = array(NA, c(itemsCount, factorsCount, iterationsCount))
  
  cat("Bootstrapping factor loadings...\n")
  
  progressBar <- txtProgressBar(min = 0, max = iterationsCount, style = 3)
  
  for(i in 1:iterationsCount) {
    newData <- data[sample(nrow(data), nrow(data), replace = TRUE),]
    bootstrappedFA = tryCatch(fa(newData, nfactors = factorsCount, fm = "ml", rotate = "none"),
                              warning = function(err) {},
                              error = function(err) {})
    
    if (!is.null(bootstrappedFA)) {
      targetLoadings = target.rot(bootstrappedFA$loadings, originalLoadings)$loadings
      
      bootstrappedLoadings[,,i] = targetLoadings
    }
    
    setTxtProgressBar(progressBar, i)
  }
  
  result = array(0, c(itemsCount, factorsCount, 3))
  
  for (i in 1:itemsCount) {
    for (j in 1:factorsCount) {
      result[i, j, 1] = mean(bootstrappedLoadings[i, j,], na.rm = TRUE)
      result[i, j, 2] = quantile(bootstrappedLoadings[i,j,], 0.025, na.rm = TRUE)
      result[i, j, 3] = quantile(bootstrappedLoadings[i,j,], 0.975, na.rm = TRUE)
    }
  }
  
  return (result)
}

# tests single dataset
efa = function(data, factorsCount) {
  scaledResponses = scale(data)
  itemsCount = dim(data)[2]
  subjectsCount = dim(data)[1]
  startTime = proc.time()
  
  cat("Classical EFA...\n")
  cat("Start time:", format(Sys.time(), "%X"), "\n")
  
  fit = fa(scaledResponses, nfactors = factorsCount, rotate = "varimax", fm = "ml", n.iter = 1)
  unrotatedFit = fa(scaledResponses, nfactors = factorsCount, rotate = "none", fm = "ml", n.iter = 1)
  factorLoadings = array(0, dim(fit$loadings))
  unrotatedLoadings = array(0, dim(fit$loadings))
  
  bootstrappedLoadings = bootstrapFactorLoadings(scaledResponses, factorsCount, itemsCount, iterationsCount = 1000)
  
  for (i in 1:itemsCount) {
    for (j in 1:factorsCount) {
      factorLoadings[i, j] = fit$loadings[i, j]
      unrotatedLoadings[i, j] = unrotatedFit$loadings[i, j]
    }
  }
  
  bootstrappedLoadings[,, 1] = factorLoadings
  
  # calculate explained variance
  if(is.null(fit$Phi)) {
    if(factorsCount > 1) {
      vx = colSums(fit$loadings^2)
    } else {
      vx = sum(fit$loadings^2)
    }
  } else {
    vx = diag(fit$Phi %*% t(load) %*% load)
  }
  
  if (factorsCount > 1) {
    if (is.null(fit$Phi)) {
      h2 = rowSums(fit$loadings ^ 2)
    } else {
      h2 = diag(fit$loadings %*% fit$Phi %*% t(fit$loadings))
    }
  } else {
    h2 = fit$loadings ^ 2
  }
  
  if(!is.null(fit$uniquenesses)) {
    u2 = fit$uniquenesses
  } else {
    u2 = (1 - h2)
  }
  
  vtotal =sum(h2 + u2)
  names(vx) = colnames(fit$loadings)
  explainedVariance = rbind("SS loadings" = vx)
  explainedVariance = rbind(explainedVariance, "Proportion Var" = vx / vtotal)
  
  if (factorsCount > 1) {
    explainedVariance = rbind(explainedVariance, "Cumulative Var" =  cumsum(vx / vtotal))
    explainedVariance = rbind(explainedVariance, "Proportion Explained" =  vx / sum(vx))
    explainedVariance = rbind(explainedVariance, "Cumulative Proportion" =  cumsum(vx / sum(vx)))
  }
  
  cat("\nDone!\n")
  
  endTime = proc.time()
  duration = (endTime - startTime)[3]
  
  cat("Elapsed time:", duration, "s\n")
  
  return (list("fit" = fit,
               "factorLoadings" = bootstrappedLoadings,
               "unrotatedLoadings" = unrotatedLoadings,
               "rotationMatrix" = fit$rot.mat,
               "eigenvalues" = fit$e.values,
               "communalities" = fit$communality,
               "explainedVariance" = explainedVariance,
               "pValue" = fit$PVAL,
               "chi2" = fit$STATISTIC,
               "df" = fit$dof,
               "RMSEA" = fit$RMSEA,
               "RMSR" = fit$rms,
               "CFI" = ((fit$null.chisq - fit$null.dof) - (fit$STATISTIC - fit$dof)) / (fit$null.chisq - fit$null.dof),
               "TLI" = fit$TLI,
               "BIC" = fit$BIC,
               "duration" = duration))
}
