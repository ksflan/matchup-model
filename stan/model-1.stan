

data {
  int<lower=0> N; // num observations
  int<lower=1> B; // num distinct batters
  int<lower=1> P; // num distinct pitchers
  int<lower=0> K; // num covariates
  int<lower=1,upper=B> batter[N]; // batter index
  int<lower=1,upper=P> pitcher[N]; // pitcher index
  int<lower=0,upper=1> outcome[N]; // atbat outcome
  // int<lower=0,upper=1> party[L];
  // matrix[B,K] W; // covariate matrix
  matrix[N,K] V; // atbat-level covariates
}
parameters {
  vector[K] mu_theta;
  
  vector[K] theta_star[B]; // batter intercept
  real beta[P]; // pitcher intercept
  vector<lower=0>[K] sigma_theta; // batter variance
  // vector[K] lambda; // covariate coefficients
  // real lambda_0;
  // real<lower=1> lambda_1;
  
  // real eta[2];
  
  // vector<lower=0>[K] tau;
  // corr_matrix[K] omega;
  // vector[K] mu_lambda;
}
transformed parameters {
  vector[K] theta[B]; // batter intercept
  
  for(b in 1:B) {
    for(k in 1:K) {
      theta[b][k] = mu_theta[k] + sigma_theta[k] * theta_star[b][k];
    }
  }
}
model {
  
  // eta ~ normal(0,5);
  
  // lambda ~ normal(0,1);
  // lambda[2] ~ normal(-0.5,0.001);
  
  // mu_lambda ~ normal(0,0.001);
  // mu_lambda[2] ~ normal(1,0.01);
  // lambda ~ multi_normal(mu_lambda, quad_form_diag(omega, tau));
  // tau ~ cauchy(0, 2.5);
  // omega ~ lkj_corr(2);
  // lambda_0 ~ normal(0,5);
  // lambda_1 ~ cauchy(0,5);
  
  beta ~ normal(0,1);
  sigma_theta ~ cauchy(0,5);
  // sigma ~ normal(0.05,0.0001);
  
  mu_theta ~ normal(0,1);
  
  for(b in 1:B) {
    for(k in 1:K) {
      theta_star[b][k] ~ normal(0,1);
    }
  }

  
  for(n in 1:N)
    outcome[n] ~ bernoulli_logit(V[n]*theta[batter[n]] - beta[pitcher[n]]);
}






