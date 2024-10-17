data {
  
  // omega (probability of biting a human given a bite)
  int omega_ancestry_N;                                         // number of observations
  vector[omega_ancestry_N] omega_ancestry;                      // vector of prob. biting human | ancestry
  vector[omega_ancestry_N] omega_ancestry_aa;                   // vector of proportion aa ancestry

  // alpha (biting rate)
  int alpha_climate_N;                                          // number of observations
  vector[alpha_climate_N] alpha_climate_temp;                   // vector of temperatures
  vector[alpha_climate_N] alpha_climate;                        // vector of trait alpha

  // b (prob mosquito infectiousness)
  int b_climate_N;                                              // number of observations
  vector[b_climate_N] b_climate_temp;                           // vector of temperatures
  vector[b_climate_N] b_climate;                                // vector of trait b

  // EIR (extrinsic incubation rate)
  int EIR_climate_N;                                            // number of observations
  vector[EIR_climate_N] EIR_climate_temp;                       // vector of temperatures
  vector[EIR_climate_N] EIR_climate;                            // vector of trait EIR

  // lifespan (1/mosquito mortality rate)
  int lf_climate_N;                                             // number of observations
  vector[lf_climate_N] lf_climate_temp;                         // vector of temperatures
  vector[lf_climate_N] lf_climate;                              // vector of trait lf

  // pMI ancestry (prob mosquito infection)
  int pMI_ancestry_N;                                           // number of observations
  vector[pMI_ancestry_N] pMI_ancestry;                          // vector of prob. mosquito infectiousness | ancestry
  vector[pMI_ancestry_N] pMI_ancestry_aa;                       // vector of proportion aa ancestry

  // pMI climate (prob mosquito infection)
  int pMI_climate_N;                                            // number of observations
  vector[pMI_climate_N] pMI_climate_temp;                       // vector of temperatures
  vector[pMI_climate_N] pMI_climate;                            // vector of trait pMI

  // new data
  int N_new;
  vector[N_new] temp_new;
  vector[N_new] aa_new;
}

parameters {

  // omega (probability of biting a human given a bite)
  real<lower=0> omega_ancestry_constant;                        // parameter c, lower limit
  real<lower=0, upper=1> omega_ancestry_d;                      // parameter d, upper limit
  real<lower=0> omega_ancestry_e;                               // parameter e, dose responding to halfway between c and d
  real<lower=0> omega_ancestry_sigma;                           // noise

  // alpha (biting rate)
  real<lower=0, upper=0.01> alpha_climate_constant;             // parameter c
  real<lower=0, upper=24> alpha_climate_Tmin;                   // parameter Tmin
  real<lower=24, upper=45> alpha_climate_Tmax;                  // parameter Tmax
  real<lower=0> alpha_climate_sigma;                            // noise

  // b (prob mosquito infectiousness)
  real<lower=0, upper=0.01> b_climate_constant;                 // parameter c
  real<lower=0, upper=24> b_climate_Tmin;                       // parameter Tmin
  real<lower=0> b_climate_sigma;                                // noise

  // EIR (extrinsic incubation rate)
  real<lower=0, upper=0.01> EIR_climate_constant;               // parameter c
  real<lower=0, upper=24> EIR_climate_Tmin;                     // parameter Tmin
  real<lower=24, upper=60> EIR_climate_Tmax;                    // parameter Tmax
  real<lower=0> EIR_climate_sigma;                              // noise

  // lifespan (1/mosquito mortality rate)
  real<lower=-0.5, upper=0.5> lf_climate_constant;              // parameter c
  real<lower=0, upper=24> lf_climate_Tmin;                      // parameter Tmin
  real<lower=24, upper=45> lf_climate_Tmax;                     // parameter Tmax
  real<lower=0> lf_climate_sigma;                               // noise

  // pMI ancestry (prob mosquito infection)
  real<lower=0> pMI_ancestry_constant;                          // parameter c, lower limit
  real<lower=0> pMI_ancestry_d;                                 // parameter d, upper limit
  real<lower=0> pMI_ancestry_e;                                 // parameter e, dose responding to halfway between c and d
  real<lower=0> pMI_ancestry_sigma;                             // noise

  // pMI climate (prob mosquito infection)
  real<lower=0, upper=10> pMI_climate_rmax;                     // parameter rmax
  real<lower=10, upper=40> pMI_climate_Topt;                    // parameter Topt
  real<lower=0, upper=10> pMI_climate_a;                        // parameter a
  real<lower=0> pMI_climate_sigma;                              // noise
}

model {                                                         // Fit models to observed data
  // Briere equation: c*x*(x-Tmin)*sqrt(Tmax-x)
  // Guassian equation: rmax*exp(-0.5*(abs(x-Topt)/a)^2)
  // Michaelis Menten equation: c+(d-c)/(1+(e/x))
  
  // omega (probability of biting a human given a bite)
  omega_ancestry_constant ~ normal(0.15,0.1);                    // prior for c
  omega_ancestry_d ~ normal(1.5,1);                              // prior for d
  omega_ancestry_e ~ normal(0.5,0.1);                            // prior for e
  omega_ancestry_sigma ~ normal(0.01,0.01);                      // prior for sigma

  for(h in 1:omega_ancestry_N){
    real omega_ancestry_mu = omega_ancestry_constant + ((omega_ancestry_d - omega_ancestry_constant)/(1 + (omega_ancestry_e / omega_ancestry_aa[h])));
    omega_ancestry[h] ~ normal(omega_ancestry_mu, omega_ancestry_sigma);
  }

  // alpha (biting rate)
  alpha_climate_constant ~ normal(2.02e-4, 0.01);               // prior for c
  alpha_climate_Tmin ~ normal(13.35,1);                         // prior for Tmin
  alpha_climate_Tmax ~ normal(40.08,0.01);                      // prior for Tmax
  alpha_climate_sigma ~ normal(0.01,0.1);                       // prior for sigma

  for(i in 1:alpha_climate_N){
    real alpha_climate_mu = alpha_climate_constant * alpha_climate_temp[i] * (alpha_climate_temp[i] - alpha_climate_Tmin) * sqrt(alpha_climate_Tmax - alpha_climate_temp[i]);
    alpha_climate[i] ~ normal(alpha_climate_mu, alpha_climate_sigma);
  }
  if (alpha_climate_Tmin > alpha_climate_Tmax) {
    target += positive_infinity();
  }

  // b (prob mosquito infectiousness)
  b_climate_constant ~ normal(5.5E-04,0.1);                    // prior for c
  b_climate_Tmin ~ normal(12,2);                               // prior for Tmin
  b_climate_sigma ~ normal(0.01,0.1);                          // prior for sigma

  for(j in 1:b_climate_N){
    real b_climate_mu = b_climate_constant * b_climate_temp[j] * (b_climate_temp[j] - b_climate_Tmin) * sqrt(38 - b_climate_temp[j]);
    b_climate[j] ~ normal(b_climate_mu, b_climate_sigma);
  }

  // EIR (extrinsic incubation rate)
  EIR_climate_constant ~ normal(6.65E-05,0.1);                  // prior for c
  EIR_climate_Tmin ~ normal(10.68,1);                           // prior for Tmin
  EIR_climate_Tmax ~ normal(45.90,1);                           // prior for Tmax
  EIR_climate_sigma ~ normal(0.01,0.1);                         // prior for sigma

  for(l in 1:EIR_climate_N){
    real EIR_climate_mu = EIR_climate_constant * EIR_climate_temp[l] * (EIR_climate_temp[l] - EIR_climate_Tmin) * sqrt(EIR_climate_Tmax - EIR_climate_temp[l]);
    EIR_climate[l] ~ normal(EIR_climate_mu, EIR_climate_sigma);
  }
  if (EIR_climate_Tmin > EIR_climate_Tmax) {
    target += positive_infinity();
  }

  // lifespan (1/mosquito mortality rate)
  lf_climate_constant ~ normal(-1.48E-01,0.1);                  // prior for c
  lf_climate_Tmin ~ normal(9.16,1);                             // prior for Tmin
  lf_climate_Tmax ~ normal(37.73,1);                            // prior for Tmax
  lf_climate_sigma ~ normal(0.01,0.1);                          // prior for sigma

  for(m in 1:lf_climate_N){
      real lf_climate_mu = lf_climate_constant * lf_climate_temp[m] * (lf_climate_temp[m] - lf_climate_Tmin) * sqrt(lf_climate_Tmax - lf_climate_temp[m]);
      lf_climate[m] ~ normal(lf_climate_mu, lf_climate_sigma);
  }
  if (lf_climate_Tmin > lf_climate_Tmax) {
    target += positive_infinity();
  }

  // pMI ancestry (prob mosquito infection)
  pMI_ancestry_constant ~ normal(0.15,0.1);                       // prior for c
  pMI_ancestry_d ~ normal(0.5,0.1);                               // prior for d
  pMI_ancestry_e ~ normal(0.5,0.1);                               // prior for e
  pMI_ancestry_sigma ~ normal(0.01,0.01);                         // prior for sigma

  for(kk in 1:pMI_ancestry_N){
    real pMI_ancestry_mu = pMI_ancestry_constant + ((pMI_ancestry_d - pMI_ancestry_constant)/(1 + (pMI_ancestry_e / pMI_ancestry_aa[kk])));
    pMI_ancestry[kk] ~ normal(pMI_ancestry_mu, pMI_ancestry_sigma);
  }

  // pMI climate (prob mosquito infection)
  pMI_climate_rmax ~ normal(0.24, 0.1);                          // prior for rmax
  pMI_climate_Topt ~ normal(30.08, 1);                           // prior for Topt
  pMI_climate_a ~ normal(3.60, 1);                               // prior for a
  pMI_climate_sigma ~ normal(0.01,0.1);                          // prior for sigma

  for(k in 1:pMI_climate_N){
    real pMI_climate_mu = pMI_climate_rmax * exp(-0.5 * (fabs(pMI_climate_temp[k] - pMI_climate_Topt)/pMI_climate_a)^2);
    pMI_climate[k] ~ normal(pMI_climate_mu, pMI_climate_sigma);
  }

}

generated quantities {

  // parameters for posterior predictive checks
  real omega_ancestry_ppc[omega_ancestry_N];                      // omega (prob biting human)
  real alpha_climate_ppc[alpha_climate_N];                        // alpha (biting rate)
  real b_climate_ppc[b_climate_N];                                // b (prob mosquito infectiousness)
  real EIR_climate_ppc[EIR_climate_N];                            // EIR (extrinsic incubation rate)
  real lf_climate_ppc[lf_climate_N];                              // lifespan (1/mosquito mortality rate)
  real pMI_ancestry_ppc[pMI_ancestry_N];                          // pMI (prob mosquito infection)
  real pMI_climate_ppc[pMI_climate_N];                            // pMI (prob mosquito infection)

  // new parameters for predictions
  vector[N_new] NmNh_new;                                         // ratio of mosquitoes to humans
  vector[N_new] delta_new;                                        // intrinsic incubation period
  vector[N_new] mu_h_new;                                         // human mortality rate
  vector[N_new] gamma_new;                                        // Human infectivity period
  vector[N_new] alpha_new;                                        // alpha (biting rate)
  vector[N_new] b_new;                                            // b (prob mosquito infectiousness)
  vector[N_new] EIR_new;                                          // EIR (extrinsic incubation rate)
  vector[N_new] lf_new;                                           // lifespan (1/mosquito mortality rate)
  vector[N_new] pMI_new;                                          // pMI (prob mosquito infection)
  vector[N_new] omega_ancestry_new;                               // omega (prob biting human)
  vector[N_new] alpha_climate_new;                                // alpha (biting rate)
  vector[N_new] b_climate_new;                                    // b (prob mosquito infectiousness)
  vector[N_new] EIR_climate_new;                                  // EIR (extrinsic incubation rate)
  vector[N_new] lf_climate_new;                                   // lifespan (1/mosquito mortality rate)
  vector[N_new] pMI_ancestry_new;                                 // pMI (prob mosquito infection)
  vector[N_new] pMI_climate_new;                                  // pMI (prob mosquito infection)

  // R0 models
  vector[N_new] R0_climate_new;
  vector[N_new] R0_ancestry_new;
  vector[N_new] R0_full_new;

  // posterior predictive
  // ppc omega (prob biting human)
  for (o in 1:omega_ancestry_N){
    real omega_ancestry_mu_ppc = omega_ancestry_constant + ((omega_ancestry_d - omega_ancestry_constant)/(1 + (omega_ancestry_e / omega_ancestry_aa[o])));
    omega_ancestry_ppc[o] = normal_rng(omega_ancestry_mu_ppc, omega_ancestry_sigma);
  }

  // ppc alpha (biting rate)
  for (p in 1:alpha_climate_N){
    real alpha_climate_mu_ppc = alpha_climate_constant * alpha_climate_temp[p] * (alpha_climate_temp[p] - alpha_climate_Tmin) * sqrt(alpha_climate_Tmax - alpha_climate_temp[p]);
    alpha_climate_ppc[p] = normal_rng(alpha_climate_mu_ppc, alpha_climate_sigma);
  }

  // ppc b (prob mosquito infectiousness)
  for (q in 1:b_climate_N){
    real b_climate_mu_ppc = b_climate_constant * b_climate_temp[q] * (b_climate_temp[q] - b_climate_Tmin) * sqrt(38 - b_climate_temp[q]);
    b_climate_ppc[q] = normal_rng(b_climate_mu_ppc, b_climate_sigma);
  }

  // ppc EIR (extrinsic incubation rate)
  for (s in 1:EIR_climate_N){
    real EIR_climate_mu_ppc = EIR_climate_constant * EIR_climate_temp[s] * (EIR_climate_temp[s] - EIR_climate_Tmin) * sqrt(EIR_climate_Tmax - EIR_climate_temp[s]);
    EIR_climate_ppc[s] = normal_rng(EIR_climate_mu_ppc, EIR_climate_sigma);
  }

  // ppc lifespan (1/mosquito mortality rate)
  for (u in 1:lf_climate_N){
    real lf_climate_mu_ppc = lf_climate_constant * lf_climate_temp[u] * (lf_climate_temp[u] - lf_climate_Tmin) * sqrt(lf_climate_Tmax - lf_climate_temp[u]);
    lf_climate_ppc[u] = normal_rng(lf_climate_mu_ppc, lf_climate_sigma);
  }

  // ppc pMI ancestry (prob mosquito infection)
  for (rr in 1:pMI_ancestry_N){
    real pMI_ancestry_mu_ppc = pMI_ancestry_constant + ((pMI_ancestry_d - pMI_ancestry_constant)/(1 + (pMI_ancestry_e / pMI_ancestry_aa[rr])));
    pMI_ancestry_ppc[rr] = normal_rng(pMI_ancestry_mu_ppc, pMI_ancestry_sigma);
  }

  // ppc pMI climate (prob mosquito infection)
  for (r in 1:pMI_climate_N){
    real pMI_climate_mu_ppc = pMI_climate_rmax * exp(-0.5 * (fabs(pMI_climate_temp[r] - pMI_climate_Topt)/pMI_climate_a)^2);
    pMI_climate_ppc[r] = normal_rng(pMI_climate_mu_ppc, pMI_climate_sigma);
  }

  // predict on new data
  for(zz in 1:N_new){

    // omega (prob biting | ancestry)
    omega_ancestry_new[zz] = normal_rng(omega_ancestry_constant + ((omega_ancestry_d - omega_ancestry_constant)/(1 + (omega_ancestry_e / aa_new[zz]))), omega_ancestry_sigma);

    // alpha (biting rate)
    if(alpha_climate_Tmin < temp_new[zz] && alpha_climate_Tmax > temp_new[zz]){
      alpha_climate_new[zz] = normal_rng((alpha_climate_constant * temp_new[zz] * (temp_new[zz] - alpha_climate_Tmin) * sqrt(alpha_climate_Tmax - temp_new[zz])), alpha_climate_sigma);
    }
    else {
      alpha_climate_new[zz] = 0;
    }

    // b (prob mosquito infectiousness)
    if(b_climate_Tmin < temp_new[zz] && 38 > temp_new[zz]){
      b_climate_new[zz] = normal_rng((b_climate_constant * temp_new[zz] * (temp_new[zz] - b_climate_Tmin) * sqrt(38 - temp_new[zz])), b_climate_sigma);
    }
    else {
      b_climate_new[zz] = 0;
    }

    // EIR (extrinsic incubation rate)
    if(EIR_climate_Tmin < temp_new[zz] && EIR_climate_Tmax > temp_new[zz]){
      EIR_climate_new[zz] = normal_rng((EIR_climate_constant * temp_new[zz] * (temp_new[zz] - EIR_climate_Tmin) * sqrt(EIR_climate_Tmax - temp_new[zz])), EIR_climate_sigma);
    }
    else {
      EIR_climate_new[zz] = 0;
    }

    // lifespan (1/mosquito mortality rate)
    if(lf_climate_Tmin < temp_new[zz] && lf_climate_Tmax > temp_new[zz]){
      lf_climate_new[zz] = normal_rng((lf_climate_constant * temp_new[zz] * (temp_new[zz] - lf_climate_Tmin) * sqrt(lf_climate_Tmax - temp_new[zz])), lf_climate_sigma);
    }
    else {
      lf_climate_new[zz] = 0;
    }

    // pMI ancestry (prob mosquito infection)
    pMI_ancestry_new[zz] = normal_rng(pMI_ancestry_constant + ((pMI_ancestry_d - pMI_ancestry_constant)/(1 + (pMI_ancestry_e / aa_new[zz]))), pMI_ancestry_sigma);

    // pMI (prob mosquito infection)
    pMI_climate_new[zz] = normal_rng(pMI_climate_rmax * exp(-0.5 * (fabs(temp_new[zz] - pMI_climate_Topt)/pMI_climate_a)^2), pMI_climate_sigma);

    NmNh_new[zz] = normal_rng(2,0.5);
    mu_h_new[zz] = normal_rng(4.25E-05, 0.00005);
    delta_new[zz] = 1/gamma_rng(5.9, 0.5);                  //zikv from https://journals.plos.org/plosntds/article?id=10.1371/journal.pntd.0004726
    gamma_new[zz] = 1/gamma_rng(5.0, 0.5);

    // mean = parameters values at 29C (optimal temperature for transmission based on previous studies)
    alpha_new[zz] = normal_rng(0.31, 0.05);
    b_new[zz] = normal_rng(0.77, 0.05);
    EIR_new[zz] = normal_rng(0.15, 0.05);
    lf_new[zz] = normal_rng(24.36, 2);
    pMI_new[zz] = normal_rng(0.24, 0.05);

    // R0 climate
    real temp_R0_climate = sqrt(
      (alpha_climate_new[zz] * b_climate_new[zz] *
      (EIR_climate_new[zz] / ((1/lf_climate_new[zz]) * ((1/lf_climate_new[zz]) + EIR_climate_new[zz])))) *
      (alpha_climate_new[zz] * pMI_climate_new[zz] * NmNh_new[zz] * (delta_new[zz] / ((delta_new[zz] + mu_h_new[zz]) *
      (gamma_new[zz] + mu_h_new[zz])))));

    // Check if temp_R0 is NaN, and if so, set R0_climate_new[zz] to 0
    if (is_nan(temp_R0_climate)) {
      R0_climate_new[zz] = 0;
    } else {
      R0_climate_new[zz] = temp_R0_climate;
    }

    // R0 ancestry (omega and pMI)
    real temp_R0_ancestry = sqrt(
      (omega_ancestry_new[zz] * alpha_new[zz] * b_new[zz] * (EIR_new[zz] / ((1/lf_new[zz]) * ((1/lf_new[zz]) + EIR_new[zz])))) *
      (alpha_new[zz] * pMI_ancestry_new[zz] * NmNh_new[zz] * (delta_new[zz] / ((delta_new[zz] + mu_h_new[zz]) * (gamma_new[zz] + mu_h_new[zz]))))
      );

    if (is_nan(temp_R0_ancestry)) {
      R0_ancestry_new[zz] = 0;
    } else {
      R0_ancestry_new[zz] = temp_R0_ancestry;
    }

    // R0 climate + ancestry (pMI ancestry)
    real temp_R0_full = sqrt((omega_ancestry_new[zz] * alpha_climate_new[zz] * b_climate_new[zz] *
    (EIR_climate_new[zz] / ((1/lf_climate_new[zz]) * ((1/lf_climate_new[zz]) + EIR_climate_new[zz])))) *
    (alpha_climate_new[zz] * pMI_ancestry_new[zz] * NmNh_new[zz] * (delta_new[zz] / ((delta_new[zz] + mu_h_new[zz]) *
    (gamma_new[zz] + mu_h_new[zz])))));

    if (is_nan(temp_R0_full)) {
      R0_full_new[zz] = 0;
    } else {
      R0_full_new[zz] = temp_R0_full;
    }

  }

}
