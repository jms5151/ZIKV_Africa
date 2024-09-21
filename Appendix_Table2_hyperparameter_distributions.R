# Appendix Table 2: Median & 95% CI for each hyperparameter ---------------

# requires the following functions:
# params from functions/fn_list_params.R

param95ci <- data.frame('param_name' = character(), 'ci5' = numeric(), 'ci50' = numeric(), 'ci95' = numeric())

for(i in 1:length(params)){
  cis <- quantile(list_of_draws[[params[i]]], c(0.05, 0.50, 0.95))
  param95ci <- param95ci %>% 
    add_row(param_name = params[i], 
            'ci5' = unname(cis[1]), 
            'ci50' = unname(cis[2]),
            'ci95' = unname(cis[3])
    )
}

write.csv(param95ci, 'Appendix_Table2.csv', row.names = F)
