// First attempt at a multinomial outcome model


data {
  int<lower=0> N; // num observations
  int<lower=1> B; // num distinct batters
  int<lower=1> P; // num distinct pitchers
  int<lower=0> K; // num covariates
  int<lower=1> D; // number of distinct outcomes
  // int<lower=1> S; // number of distinct venues
  int<lower=1,upper=B> batter[N]; // batter index
  int<lower=1,upper=P> pitcher[N]; // pitcher index
  int<lower=1,upper=D> outcome[N]; // atbat outcome
  // int<lower=1,upper=S> stadium[N]; // venue index
  row_vector[D] event_values;
  real zero; // this is just the number 0, to be used for fixing the Dth element of the pitcher/batter coefficient vectors
  // matrix[B,K] W; // covariate matrix
  matrix[N,K] V; // atbat-level covariates
}
parameters {
  vector[D-1] mu_theta;
  row_vector<lower=0>[D-1] sigma_theta;
  
  vector[D-1] mu_beta;
  row_vector<lower=0>[D-1] sigma_beta;
  
  vector[D-1] theta_star_raw[B];
  vector[D-1] beta_star_raw[P];
  
  vector[K] omega; // covariate coefficients (non-hierarchical for now)
}
transformed parameters {
  // vector[D-1] theta_star[B];
  vector[D] theta[B];
  // vector[D-1] beta_star[P];
  vector[D] beta[P];
  
  for(b in 1:B) {
    for(d in 1:(D-1)) {
      theta[b][d] = mu_theta[d] + sigma_theta[d] * theta_star_raw[b][d];
    }

    theta[b][D] = zero;
  }
    // theta_star[b] = mu_theta + sigma_theta * theta_star_raw[b];

  for(p in 1:P) {
    for(d in 1:(D-1)) {
      beta[p][d] = mu_beta[d] + sigma_beta[d] * beta_star_raw[p][d];
    }

    beta[p][D] = zero;
  }
  // beta_star[p] = mu_beta + sigma_beta * beta_star_raw[p];
    
  // for(b in 1:B) {
  //   for(d in 1:(D-1)) {
  //     theta[b][d] = theta_star[b][d];
  //   }
  //   theta[b][D] = -sum(theta_star[b]);
  // }
  // 
  // for(p in 1:P) {
  //   for(d in 1:(D-1)) {
  //     beta[p][d] = beta_star[p][d];
  //   }
  //   beta[p][D] = -sum(beta_star[p]);
  // }
}
model {
  mu_theta ~ normal(0,1);
  mu_beta ~ normal(0,1);
  sigma_theta ~ cauchy(0,5);
  sigma_beta ~ cauchy(0,5);
  
  for(p in 1:P)
    beta_star_raw[p] ~ normal(0,1);
  
  for(b in 1:B) {
    // for(d in 1:(D-1)) {
      theta_star_raw[b] ~ normal(0,1);
    // }
  }
  
  omega ~ normal(0,1);
  
  for(n in 1:N)
    outcome[n] ~ categorical_logit(theta[batter[n]] - beta[pitcher[n]] + V[n] * omega);
}
generated quantities {
  vector[D] batter_outcomes[B];
  vector[D] pitcher_outcomes[P];
  vector[D] mean_batter_outcomes;
  vector[D] mean_pitcher_outcomes;
  real batter_wOBA[B];
  real pitcher_wOBA[P];
  
  mean_batter_outcomes = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)));
  mean_pitcher_outcomes = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)));
  
  for(b in 1:B) {
    batter_outcomes[b] = softmax(theta[b] - append_row(mu_beta, rep_vector(0.0, 1)));
    batter_wOBA[b] = event_values * batter_outcomes[b];
  }
    
  for(p in 1:P) {
    pitcher_outcomes[p] = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - beta[p]);
    pitcher_wOBA[p] = event_values * pitcher_outcomes[p];
  }
}



