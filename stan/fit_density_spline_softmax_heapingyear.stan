data {
  int<lower=1> N;

  int num_grid;
  int num_basis;

  int min_Y[N];
  int max_Y[N];

 matrix[num_basis, num_grid] B;
}

parameters {
  row_vector[num_basis] a_raw;
  real<lower=0> tau;
  real<lower=0> a0;
}

transformed parameters {
  row_vector[num_basis] a;
  vector[num_grid] gamma_hat;
  vector[num_grid] pmf;
  vector[num_grid] lpmf;

  a = exp(cumulative_sum(a_raw * tau));
  gamma_hat = to_vector(a*B) + 0.000000001;
  pmf = gamma_hat/sum(gamma_hat);
  lpmf = log(pmf);
}

model {
  tau ~ normal(0,1);
  a_raw ~ normal(0, 1);
  a0 ~ normal(0, 3);

  for(i in 1:N) {
    target += log(sum(pmf[(min_Y[i] + 1):(max_Y[i]+1)]));
  }
}
