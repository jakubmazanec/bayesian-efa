source("modules/hdi.R")

# summarizePost = function (samples, compVal = NULL, rope = NULL , credibleMass = 0.95) {
summarizeSamples = function (samples, rope = NULL , credibleMass = 0.95) {
  samplesMean = mean(samples)
  samplesMedian = median(samples)
  samplesDensity = density(samples)
  samplesMode = samplesDensity$x[which.max(samplesDensity$y)]
  # mcmcEffSz = round( effectiveSize( samples ) , 1 )
  # names(mcmcEffSz) = NULL
  
  hdiLim = hdi(samples, credibleMass = credibleMass)
#   hdiLim = c(0, 0)
#   hdiLim[[1]] = quantile(samples, 0.025)
#   hdiLim[[2]] = quantile(samples, 0.975)
  
  # if (!is.null(compVal)) {
  #   pcgtCompVal = 100 * sum(samples > compVal) / length(samples)
  # } else {
  #   compVal = NA
  #   pcgtCompVal = NA
  # }
  
  if (!is.null(rope)) {
    pcltRope = 100 * sum(samples < rope[1]) / length(samples)
    pcgtRope = 100 * sum(samples > rope[2]) / length(samples)
    pcinRope = 100 - (pcltRope + pcgtRope)
  } else {
    rope = c(NA, NA)
    pcltRope = NA
    pcgtRope = NA
    pcinRope = NA
  }
  
  return (c(mean = samplesMean,
            median = samplesMedian,
            mode = samplesMode,
             #ESS=mcmcEffSz ,
            hdiMass = credibleMass,
            hdiLower=hdiLim[1],
            hdiUpper=hdiLim[2],
            #CompVal=compVal , PcntGtCompVal=pcgtCompVal ,
            ropeLower = rope[1],
            ropeUpper = rope[2],
            belowROPE = pcltRope,
            inROPE = pcinRope,
            aboveROPE = pcgtRope))
}
