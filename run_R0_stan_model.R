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
