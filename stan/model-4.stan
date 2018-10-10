
// OBP model with dynamic parameters



data {
  int<lower=0> N;
  int<lower=0> B;
  int<lower=0> P;
  int<lower=0> P_B; // number of total batter params
  int<lower=0> P_P; // number of total pitcher params
  int<lower=0> batter_periods[B];
  int<lower=0> pitcher_periods[P];
  int<lower=0> batter_index[N];
  int<lower=0> pitcher_index[N];
  int<lower=0> cumulative_batter_index[B];
  int<lower=0> cumulative_pitcher_index[P];
  int<lower=0> period_index_batter[N];
  int<lower=0> period_index_pitcher[N];
  int<lower=0> outcome[N];
}
parameters {
  real beta[P_B];
  real theta[P_P];
  
  real mu_beta;
  real<lower=0> sigma_beta;
  
  real<lower=0> tau_b;
  real<lower=0> tau_p;
  
}
model {
  int pos;
  
  mu_beta ~ normal(-1,1);
  sigma_beta ~ cauchy(0,5);
  
  // tau_p ~ cauchy(0,5);
  // tau_b ~ cauchy(0,5);
  tau_p ~ normal(0.2,0.01);
  tau_b ~ normal(0.2,0.01);
  
  pos = 1;
  for(b in 1:B) {
    beta[pos] ~ normal(mu_beta,sigma_beta);
    
    if(batter_periods[b] > 1) {
      for(i in 2:batter_periods[b]) {
        beta[pos + i - 1] ~ normal(beta[pos + i - 2],tau_b);
      }
    }
    
    pos = pos + batter_periods[b];
  }
  
  pos = 1;
  for(p in 1:P) {
    theta[pos] ~ normal(0,1);
    
    if(pitcher_periods[p] > 1) {
      for(i in 2:pitcher_periods[p]) {
        theta[pos + i - 1] ~ normal(theta[pos + i - 2],tau_p);
      }
    }
    
    pos = pos + pitcher_periods[p];
  }
  
  for(n in 1:N) {
    outcome[n] ~ bernoulli_logit(beta[cumulative_batter_index[batter_index[n]] + period_index_batter[n]] - theta[cumulative_pitcher_index[pitcher_index[n]] + period_index_pitcher[n]]);
  }
}


