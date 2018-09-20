// This model only models OBP
// I intend to add a dynamic component to this model



data {
  int<lower=1> N;
  int<lower=1> B;
  int<lower=1> P;
  int<lower=1> S;
  
  // these could be converted to matrices
  int<lower=1,upper=B> batter[N];
  int<lower=1,upper=B> pitcher[N];
  int<lower=1,upper=S> stadium[N];
  int<lower=0,upper=1> platoon[N];
  int<lower=0,upper=1> inning_bottom[N];
  
  int<lower=0,upper=1> onbase[N];
}
parameters {
  real mu_beta;
  real<lower=0> sigma_beta;
  
  vector[B] beta_star;
  vector[P] theta;
  
  real mu_psi;
  real<lower=0> sigma_psi;
  
  vector[S] psi_star; // stadium
  real gamma; // HFA
  real lambda; // platoon
  
  real alpha; // intercept
}
transformed parameters {
  vector[B] beta;
  vector[S] psi;
  
  for(b in 1:B)
    beta[b] = mu_beta + sigma_beta * beta_star[b];
  
  for(s in 1:S)
    psi[s] = mu_psi + sigma_psi * psi_star[s];
}
model {
  mu_beta ~ normal(0,0.001);
  sigma_beta ~ cauchy(0,5);
  beta_star ~ normal(0,1);
  
  theta ~ normal(0,1);
  
  mu_psi ~ normal(0,0.01);
  sigma_psi ~ cauchy(0,1);
  psi_star ~ normal(0,1);
  
  gamma ~ normal(0,1);
  lambda ~ normal(0,1);
  alpha ~ normal(0,1);
  
  for(n in 1:N)
    onbase[n] ~ bernoulli_logit(alpha + beta[batter[n]] - theta[pitcher[n]] + psi[stadium[n]] + gamma * inning_bottom[n] + lambda * platoon[n]);
}


