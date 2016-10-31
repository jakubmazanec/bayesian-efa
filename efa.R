# load libraries and functions
source("modules/index.R")

# dataset parameters
intervalScaleBreaks = FALSE
ordinalScaleBreaks = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5)

smallSampleSize = 100
mediumSampleSize = 250
largeSampleSize = 500

factorLoadings1 = matrix(c(0.9, 0.9, 0.9, 0.9, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 0,   0,   0,   0,
                           0,   0,   0,   0,   0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.6, 0.7, 0.7, 0.8, 0.8, 0.8, 0.9, 0.9, 0.9, 0.9),
                         nrow = 20, ncol = 2)
factorLoadings2 = matrix(c(0.6, 0.6,  0.55, 0.5,  0.45, 0.45, 0.4,  0.4,  0.4,  0.3, 0.3, 0.25, 0.3,  0.15, 0.15, 0.1,  0.1, 0.05, 0.1, 0.05,
                           0,   0.3,  0.25, 0.15, 0.2,  0.15, 0.05, 0.05, 0.05, 0.1, 0.6, 0.6,  0.55, 0.5,  0.45, 0.45, 0.4, 0.4,  0.3, 0.25),
                         nrow = 20, ncol = 2)
factorLoadings3 = matrix(c(0.6, -0.6,  0.6, -0.6, 0,    0.5, -0.1,  0.1, 0.4, -0.4,  0.4, -0.4, 0,    0.3, -0.1,  0.1, 0.2, -0.2,  0.2, -0.2,
                           0,    0.6, -0.1,  0.1, 0.5, -0.5,  0.5, -0.5, 0,    0.4, -0.1,  0.1, 0.3, -0.3,  0.3, -0.3, 0,    0.2, -0.1,  0.1),
                         nrow = 20, ncol = 2)

lessNoise = 0.5
moreNoise = 1

# generate datasets
dataIntSmall1 = generateData(smallSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntSmall1Noisy = generateData(smallSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntSmall2 = generateData(smallSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntSmall2Noisy = generateData(smallSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntSmall3 = generateData(smallSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntSmall3Noisy = generateData(smallSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntMedium1 = generateData(mediumSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntMedium1Noisy = generateData(mediumSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntMedium2 = generateData(mediumSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntMedium2Noisy = generateData(mediumSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntMedium3 = generateData(mediumSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntMedium3Noisy = generateData(mediumSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntLarge1 = generateData(largeSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntLarge1Noisy = generateData(largeSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntLarge2 = generateData(largeSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntLarge2Noisy = generateData(largeSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataIntLarge3 = generateData(largeSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = intervalScaleBreaks)
dataIntLarge3Noisy = generateData(largeSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = intervalScaleBreaks)
dataOrdSmall1 = generateData(smallSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdSmall1Noisy = generateData(smallSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdSmall2 = generateData(smallSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdSmall2Noisy = generateData(smallSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdSmall3 = generateData(smallSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdSmall3Noisy = generateData(smallSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdMedium1 = generateData(mediumSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdMedium1Noisy = generateData(mediumSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdMedium2 = generateData(mediumSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdMedium2Noisy = generateData(mediumSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdMedium3 = generateData(mediumSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdMedium3Noisy = generateData(mediumSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdLarge1 = generateData(largeSampleSize, factorLoadings1, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdLarge1Noisy  = generateData(largeSampleSize, factorLoadings1, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdLarge2 = generateData(largeSampleSize, factorLoadings2, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdLarge2Noisy = generateData(largeSampleSize, factorLoadings2, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)
dataOrdLarge3 = generateData(largeSampleSize, factorLoadings3, errorSdsMax = lessNoise, breaks = ordinalScaleBreaks)
dataOrdLarge3Noisy = generateData(largeSampleSize, factorLoadings3, errorSdsMax = moreNoise, breaks = ordinalScaleBreaks)

# perform the tests
dataIntSmall1.result = testData(dataIntSmall1)
dataIntSmall1Noisy.result = testData(dataIntSmall1Noisy)
dataIntSmall2.result = testData(dataIntSmall2)
dataIntSmall2Noisy.result = testData(dataIntSmall2Noisy)
dataIntSmall3.result = testData(dataIntSmall3)
dataIntSmall3Noisy.result = testData(dataIntSmall3Noisy)
dataIntMedium1.result = testData(dataIntMedium1)
dataIntMedium1Noisy.result = testData(dataIntMedium1Noisy)
dataIntMedium2.result = testData(dataIntMedium2)
dataIntMedium2Noisy.result = testData(dataIntMedium2Noisy)
dataIntMedium3.result = testData(dataIntMedium3)
dataIntMedium3Noisy.result = testData(dataIntMedium3Noisy)
dataIntLarge1.result = testData(dataIntLarge1)
dataIntLarge1Noisy.result = testData(dataIntLarge1Noisy)
dataIntLarge2.result = testData(dataIntLarge2)
dataIntLarge2Noisy.result = testData(dataIntLarge2Noisy)
dataIntLarge3.result = testData(dataIntLarge3)
dataIntLarge3Noisy.result = testData(dataIntLarge3Noisy)
dataOrdSmall1.result = testData(dataOrdSmall1)
dataOrdSmall1Noisy.result = testData(dataOrdSmall1Noisy)
dataOrdSmall2.result = testData(dataOrdSmall2)
dataOrdSmall2Noisy.result = testData(dataOrdSmall2Noisy)
dataOrdSmall3.result = testData(dataOrdSmall3)
dataOrdSmall3Noisy.result = testData(dataOrdSmall3Noisy)
dataOrdMedium1.result = testData(dataOrdMedium1)
dataOrdMedium1Noisy.result = testData(dataOrdMedium1Noisy)
dataOrdMedium2.result = testData(dataOrdMedium2)
dataOrdMedium2Noisy.result = testData(dataOrdMedium2Noisy)
dataOrdMedium3.result = testData(dataOrdMedium3)
dataOrdMedium3Noisy.result = testData(dataOrdMedium3Noisy)
dataOrdLarge1.result = testData(dataOrdLarge1)
dataOrdLarge1Noisy.result = testData(dataOrdLarge1Noisy)
dataOrdLarge2.result = testData(dataOrdLarge2)
dataOrdLarge2Noisy.result = testData(dataOrdLarge2Noisy)
dataOrdLarge3.result = testData(dataOrdLarge3)
dataOrdLarge3Noisy.result = testData(dataOrdLarge3Noisy)
