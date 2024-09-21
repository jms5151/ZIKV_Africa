# --------------------------------------------------------
# platform       x86_64-w64-mingw32               
# arch           x86_64                           
# os             mingw32                          
# crt            ucrt                             
# system         x86_64, mingw32                  
# status                                          
# major          4                                
# minor          3.2                              
# year           2023                             
# month          10                               
# day            31                               
# svn rev        85441                            
# language       R                                
# version.string R version 4.3.2 (2023-10-31 ucrt)
# nickname       Eye Holes 
# --------------------------------------------------------

# Master code to assess model fit, create figures, and tables
# All sourced files require the model object (r0_mod) and extracted samples (list_of_draws), and many require data (mod_data)
# Many scripts require the same libraries and functions

# load project libraries
library(rstan)
library(boot)
library(matrixStats)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(egg)
library(cowplot)
library(rnaturalearth)
library(latticeExtra)
library(viridisLite)
library(ggpubr)

# open model
r0_mod <- readRDS('../models/stan_model_fit_zikv.rds')

# load data
load('../VBD-data/model_data_zikv.RData')
mod_data <- model_data_zikv

# extract samples 
list_of_draws <- rstan::extract(r0_mod)

# call functions
function_files <- list.files('functions/', full.names = T)
invisible(sapply(function_files, source, .GlobalEnv))

# Figure 1 in main text and Appendix Figure 1
source('Figure1_and_Appendix_Fig1.R')

# Figure 2
source('Figure2.R')

# Figure 3
source('Figure3.R')

# Appendix Table 2: Hyperparameter distributions
source('Appendix_Table2_hyperparameter_distributions.R')

# Appendix Table 3: R-hat & Effective Sample Size
source('Appendix_Table3_Rhat_ESS.R') 

# Appendix Figure 2: Prior vs posterior hyperparameter distributions
source('Appendix_Fig2_prior_v_poster.R')

# Appendix Figure 3: Trace plots
source('Appendix_Fig3_trace_plots.R')

# Appendix Figure 4: Trait fits
source('Appendix_Fig4_trait_fits.R')

# Appendix Figure 5: Posterior predictive checks plots
source('Appendix_Fig5_ppc_plots.R')

# Appendix Figures 6 & 7: decomposition of uncertainty between model parameters and climate and ancestry
source('Appendix_Fig6-7_decompose_uncertainty.R')

