// Takes model-2 and adds covariates (this serves as a scratch file) 


data {
  int<lower=0> N; // num observations
  int<lower=1> B; // num distinct batters
  int<lower=1> P; // num distinct pitchers
  // int<lower=0> K; // num covariates
  int<lower=1> D; // number of distinct outcomes
  int<lower=1> S; // number of distinct venues
  int<lower=1,upper=B> batter[N]; // batter index
  int<lower=1,upper=P> pitcher[N]; // pitcher index
  int<lower=1,upper=D> outcome[N]; // atbat outcome
  int<lower=1,upper=S> stadium[N]; // venue index
  row_vector[D] event_values;
  real zero; // this is just the number 0, to be used for fixing the Dth element of the pitcher/batter coefficient vectors
  // matrix[B,K] W; // covariate matrix
  vector<lower=0,upper=1>[N] platoon; // vector of platoon advantage
  // matrix[N,S] venue_matrix; // matrix of indicators for venue
  vector<lower=0,upper=1>[N] home_advantage; // indicator for whether the batter is at home
  // vector[K] V[N]; // atbat-level covariates
}
transformed data {
  row_vector[S] stadium_zeroes;
  stadium_zeroes = rep_row_vector(0.0, S);
}
parameters {
  // vector[D-1] alpha_raw; // league average
  
  vector[D-1] mu_theta;
  row_vector<lower=0>[D-1] sigma_theta;
  
  vector[D-1] mu_beta;
  row_vector<lower=0>[D-1] sigma_beta;
  
  vector[D-1] theta_star_raw[B];
  vector[D-1] beta_star_raw[P];
  
  vector[D-1] mu_omega_venue;
  vector<lower=0>[D-1] sigma_omega_venue;
  // 
  vector[D-1] omega_platoon_raw;
  vector[D-1] omega_home_raw;
  vector[D-1] omega_venue_raw[S]; // venue covariate coefficients (non-hierarchical for now)
}
transformed parameters {
  // vector[D] alpha; // league average outcomes
  
  // vector[D-1] theta_star[B];
  vector[D] theta[B];
  // vector[D-1] beta_star[P];
  vector[D] beta[P];
  vector[D] omega_platoon;
  vector[D] omega_home;
  vector[D] omega_venue[S];
  // vector[D] omega_venue_final[S];
  
  for(d in 1:(D-1)) {
    omega_platoon[d] = omega_platoon_raw[d];
    omega_home[d] = omega_home_raw[d];
    // alpha[d] = alpha_raw[d];
  }
  // 
  omega_platoon[D] = zero;
  omega_home[D] = zero;
  // alpha[D] = zero;
  
  for(s in 1:S) {
    for(d in 1:(D-1)) {
      omega_venue[s][d] = mu_omega_venue[d] + sigma_omega_venue[d] * omega_venue_raw[s][d];
    }

    omega_venue[s][D] = zero;
  }
  
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
  
  // omega_venue_final = append_row(omega_venue, stadium_zeroes);
}
model {
  mu_theta ~ normal(0,5);
  mu_beta ~ normal(0,5);
  mu_omega_venue ~ normal(0,5);
  sigma_theta ~ exponential(1.5);
  sigma_beta ~ exponential(1.5);
  sigma_omega_venue ~ exponential(1.5);
  
  for(p in 1:P)
    beta_star_raw[p] ~ normal(0,1);
  
  for(b in 1:B) {
    // for(d in 1:(D-1)) {
      theta_star_raw[b] ~ normal(0,1);
    // }
  }
  
  to_vector(omega_platoon_raw) ~ normal(0,5);
  to_vector(omega_home_raw) ~ normal(0,5);
  // to_vector(alpha_raw) ~ normal(0,5);
  for(s in 1:S) {
    for(d in 1:(D-1)) {
      omega_venue_raw[s][d] ~ normal(0,1);
    }
  }
  
  for(n in 1:N)
    outcome[n] ~ categorical_logit(theta[batter[n]] - beta[pitcher[n]] + home_advantage[n] * omega_home + platoon[n] * omega_platoon + omega_venue[stadium[n]]);
}
generated quantities {
  vector[D] batter_outcomes[B];
  vector[D] pitcher_outcomes[P];
  vector[D] stadium_outcomes[S];
  vector[D] mean_batter_outcomes;
  vector[D] mean_pitcher_outcomes;
  vector[D] mean_stadium_outcomes;
  real batter_wOBA[B];
  real pitcher_wOBA[P];
  real stadium_wOBA[S];

  mean_batter_outcomes = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)) + append_row(mu_omega_venue, rep_vector(0.0, 1)));
  mean_pitcher_outcomes = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)) + append_row(mu_omega_venue, rep_vector(0.0, 1)));
  mean_stadium_outcomes = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)) + append_row(mu_omega_venue, rep_vector(0.0, 1)));

  for(b in 1:B) {
    batter_outcomes[b] = softmax(theta[b] - append_row(mu_beta, rep_vector(0.0, 1)) + append_row(mu_omega_venue, rep_vector(0.0, 1)));
    batter_wOBA[b] = event_values * batter_outcomes[b];
  }

  for(p in 1:P) {
    pitcher_outcomes[p] = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - beta[p] + append_row(mu_omega_venue, rep_vector(0.0, 1)));
    pitcher_wOBA[p] = event_values * pitcher_outcomes[p];
  }
  
  for(s in 1:S) {
    stadium_outcomes[s] = softmax(append_row(mu_theta, rep_vector(0.0, 1)) - append_row(mu_beta, rep_vector(0.0, 1)) + omega_venue[s]);
    stadium_wOBA[s] = event_values * stadium_outcomes[s];
  }
}



