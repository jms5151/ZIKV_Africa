# load libraries
library(rstan)
library(parallel)

# load data (source = 'format_data_for_R0_stan_model.R)
load('model_data_zikv.RData')
 
# fit model
stan_model_fit_zikv <- 
  sampling(
    stan_model('R0_model.stan')
  , data = model_data_zikv
  , iter = 10000
  , cores = parallel::detectCores()-1
  )

# save stanfit object
# this is not stored on github because the model object is too large
saveRDS(stan_model_fit_zikv,'stan_model_fit_zikv.rds')

r0_mod <- stan_model_fit_zikv
list_of_draws <- rstan::extract(r0_mod)
pairs(r0_mod, pars = c('omega_ancestry_constant', 'omega_ancestry_d', 'omega_ancestry_e', 'omega_ancestry_sigma'))
pairs(r0_mod, pars = c('alpha_climate_constant', 'alpha_climate_Tmin', 'alpha_climate_Tmax', 'alpha_climate_sigma'))
pairs(r0_mod, pars = c("b_climate_constant", "b_climate_Tmin", 'b_climate_Tmax', "b_climate_sigma"))
pairs(r0_mod, pars = c('EIR_climate_constant', 'EIR_climate_Tmin', 'EIR_climate_Tmax', 'EIR_climate_sigma'))
pairs(r0_mod, pars = c('lf_climate_constant', 'lf_climate_Tmin', 'lf_climate_Tmax', 'lf_climate_sigma'))
pairs(r0_mod, pars = c('pMI_ancestry_constant', 'pMI_ancestry_d', 'pMI_ancestry_e', 'pMI_ancestry_sigma'))
pairs(r0_mod, pars = c('pMI_climate_rmax', 'pMI_climate_Topt', 'pMI_climate_a', 'pMI_climate_sigma'))
