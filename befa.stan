data {
  int<lower = 1> subjectsCount;
  int<lower = 1> itemsCount;
  int<lower = 1> factorsCount;
  matrix[subjectsCount, itemsCount] responses;
}

transformed data {
  vector[itemsCount] responseMeans;
  
  responseMeans = rep_vector(0.0, itemsCount);
}

parameters {
  matrix[itemsCount, factorsCount] factorLoadings;
  vector<lower = 0>[itemsCount] itemVariances;
  real<lower = 0> itemVariancesMean;
  real<lower = 0> itemVariancesSd;
}

transformed parameters{
  cov_matrix[itemsCount] covarianceMatrix;
  
  covarianceMatrix = factorLoadings * factorLoadings' + diag_matrix(itemVariances);
}

model {
  // the hyperpriors
  itemVariancesMean ~ cauchy(0, 1);
  itemVariancesSd ~ cauchy(0, 1);

  // the priors
  itemVariances ~ cauchy(itemVariancesMean, itemVariancesSd);
  
  for(itemIndex in 1:itemsCount) {
    factorLoadings[itemIndex] ~ cauchy(0, 1);
  }
  
  // the likelihood
  for(subjectIndex in 1:subjectsCount) {
    responses[subjectIndex] ~ multi_normal(responseMeans, covarianceMatrix);
  }
}
