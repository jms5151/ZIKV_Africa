# Zika virus in Africa

## Description
  - The code provided supports the study titled 'Vector competence and biting behaviour shape the present and future of Zika virus transmission patterns in Africa'. This code runs alternative models for predicting R0, based on traits that vary with mosquito ancestry (i.e., genetic population variation) and climate.
  - This project was run using R v4.1.0 and STAN v2.33 (https://mc-stan.org/users/documentation/)

## Authors
- Jamie M. Caldwell, Louis Lambrechts, and Noah Rose

## Files description
Data:<br>
  - model_data_zikv.RData (data used to run model)<br>
  - seroSurveys_Aaa_v2.csv (seroprevalence data) <br>

Model:<br>
  - R0_model.stan<br>

Code to run model:<br>
  - run_R0_stan_model.R<br>

Code to evaluate model fit and create figures: assess_fit_and_create_figures.R<br>
  - This is a master code to run all other codes in repository (model diagnostics, figures and table creation)<br>  
  - All codes sourced in the master file are labeled by their corresponding table or figure in the manuscript<br>  
  - This files loads all libraries and files used in multiple codes<br>  

Original (and sometime messy!) exploratory analyses can be found in a complementary repository 'https://github.com/jms5151/VBD_R0_constraints_Africa'
