# tests single dataset
testData = function(data) {
  datasetName = deparse(substitute(data))
  
  cat("Testing dataset:", datasetName, "\n")
  
  scaledResponses = scale(data$response)
  factorsCount = data$factorsCount
  itemsCount = data$itemsCount
  subjectsCount = data$subjectsCount
  trueFactorLoadings = data$factorLoadings
  
  classicFit = efa(scaledResponses, factorsCount = factorsCount)
  classicFit.factorLoadings = reorientLoadings(classicFit$factorLoadings, trueFactorLoadings)
  
  bayesFit = befa(scaledResponses, factorsCount = factorsCount)
  bayesFit.factorLoadings = reorientLoadings(bayesFit$factorLoadings, trueFactorLoadings)
  
  classicFit.ranges = classicFit.factorLoadings[,, 3] - classicFit.factorLoadings[,, 2]
  classicFit.deltas = sqrt((classicFit.factorLoadings[,, 1] - trueFactorLoadings) ^ 2)
  classicFit.hits = (trueFactorLoadings >= classicFit.factorLoadings[,, 2] & trueFactorLoadings <= classicFit.factorLoadings[,, 3])
  
  bayesFit.ranges = bayesFit.factorLoadings[,, 3] - bayesFit.factorLoadings[,, 2]
  bayesFit.deltas = sqrt((bayesFit.factorLoadings[,, 1] - trueFactorLoadings) ^ 2)
  bayesFit.hits = (trueFactorLoadings >= bayesFit.factorLoadings[,, 2] & trueFactorLoadings <= bayesFit.factorLoadings[,, 3])
  
  # save results
  bayesResults.loadings = cbind(bayesFit.factorLoadings[,, 1],
                                bayesFit.factorLoadings[,, 2],
                                bayesFit.factorLoadings[,, 3],
                                trueFactorLoadings)
  colnames(bayesResults.loadings) = c(paste("Factor", as.character(c(1:factorsCount)), "mean"),
                                      paste("Factor", as.character(c(1:factorsCount)), "2,5 %"),
                                      paste("Factor", as.character(c(1:factorsCount)), "97,5 %"),
                                      paste("Factor", as.character(c(1:factorsCount)), "true value"))
  
  write.table(bayesResults.loadings,
              file = paste0("output/", datasetName, "-bayesianEFA-factorLoadings.csv"),
              sep = ";",
              dec = ",",
              row.names = FALSE)
  
  bayesResults.assessment = cbind(bayesFit.deltas,
                                  bayesFit.hits,
                                  bayesFit.ranges)
  colnames(bayesResults.assessment) = c(paste("Factor", as.character(c(1:factorsCount)), "error"),
                                        paste("Factor", as.character(c(1:factorsCount)), "in 95% interval"),
                                        paste("Factor", as.character(c(1:factorsCount)), "95% interval width"))
  
  write.table(bayesResults.assessment,
              file = paste0("output/", datasetName, "-bayesianEFA-assessment.csv"),
              sep = ";",
              dec = ",",
              row.names = FALSE)
  
  classicResults.loadings = cbind(classicFit.factorLoadings[,, 1],
                                  classicFit.factorLoadings[,, 2],
                                  classicFit.factorLoadings[,, 3],
                                  trueFactorLoadings)
  colnames(classicResults.loadings) = c(paste("Factor", as.character(c(1:factorsCount)), "mean"),
                                        paste("Factor", as.character(c(1:factorsCount)), "2,5 %"),
                                        paste("Factor", as.character(c(1:factorsCount)), "97,5 %"),
                                        paste("Factor", as.character(c(1:factorsCount)), "true value"))
  
  write.table(classicResults.loadings,
              file = paste0("output/", datasetName, "-classicEFA-factorLoadings.csv"),
              sep = ";",
              dec = ",",
              row.names = FALSE)
  
  classicResults.assessment = cbind(classicFit.deltas,
                                    classicFit.hits,
                                    classicFit.ranges)
  colnames(classicResults.assessment) = c(paste("Factor", as.character(c(1:factorsCount)), "error"),
                                          paste("Factor", as.character(c(1:factorsCount)), "in 95% interval"),
                                          paste("Factor", as.character(c(1:factorsCount)), "95% interval width"))
  
  write.table(classicResults.assessment,
              file = paste0("output/", datasetName, "-classicEFA-assessment.csv"),
              sep = ";",
              dec = ",",
              row.names = FALSE)
  
  stats = rbind(classicFit$explainedVariance[3, factorsCount],
                classicFit$pValue,
                classicFit$chi2,
                classicFit$df,
                classicFit$RMSEA[1],
                classicFit$RMSR,
                classicFit$CFI,
                classicFit$TLI,
                classicFit$BIC,
                classicFit$duration,
                bayesFit$duration,
                mean(classicFit.deltas),
                mean(bayesFit.deltas),
                mean(classicFit.ranges),
                mean(bayesFit.ranges),
                sum(classicFit.hits / (factorsCount * itemsCount)),
                sum(bayesFit.hits / (factorsCount * itemsCount)))
  rownames(stats) = c("explainedVariance",
                      "pValue",
                      "Chi2",
                      "DF",
                      "RMSEA",
                      "RMSR",
                      "CFI",
                      "TLI",
                      "BIC",
                      "classic EFA duration",
                      "bayesian EFA duration",
                      "classic EFA mean error",
                      "bayesian EFA mean error",
                      "classic EFA mean 95% interval width",
                      "bayesian EFA mean 95% interval width",
                      "classic EFA % of items in 95% interval",
                      "bayesian EFA % of items in 95% interval")
  colnames(stats) = c("Value")
  
  write.table(stats,
              file = paste0("output/", datasetName, "-stats.csv"),
              sep = ";",
              dec = ",",
              col.names = FALSE)
  
  return (list("factorLoadings" = trueFactorLoadings,
               "factorsCount" = factorsCount,
               "itemsCount" = itemsCount,
               "subjectsCount" = subjectsCount,
               "classicFit" = classicFit,
               "classicFit.factorLoadings" = classicFit.factorLoadings,
               "classicFit.ranges" = classicFit.ranges,
               "classicFit.deltas" = classicFit.deltas,
               "classicFit.hits" = classicFit.hits,
               "bayesFit" = bayesFit,
               "bayesFit.factorLoadings" = bayesFit.factorLoadings,
               "bayesFit.ranges" = bayesFit.ranges,
               "bayesFit.deltas" = bayesFit.deltas,
               "bayesFit.hits" = bayesFit.hits,
               "datasetName" = datasetName))
}
