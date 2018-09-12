

data {
  int<lower=1> N; // number of observations
  int<lower=1> T; // number of teams
  int<lower=0> K; // number of statistics in model
  int<lower=0> Y; // number of years
  matrix[N,K] X; // matrix of rate statistics (per game)
  vector<lower=0>[N] y; // vector of run rates (per game)
  int year[N]; // the year of a given observation
}
parameters {
  matrix[Y,K] beta;
  real<lower=0> sigma; // within year variance
  vector<lower=0>[K] tau; // year-to-year transition variance
}
model {
  to_vector(beta[1]) ~ normal(0,1);
  beta[1,1] ~ normal(0,10);
  
  for(yr in 2:Y)
    for(k in 1:K)
      beta[yr,k] ~ normal(beta[yr-1,k],tau[k]);
  
  sigma ~ cauchy(0,5);
  tau ~ cauchy(0,5);
  
  for(n in 1:N)
    y[n] ~ normal(X[n] * to_vector(beta[year[n]]), sigma);
}


