# Appendix Fig. 4: Trait fits -----------------------------------------

# requires the following functions:
# plotParameterSamples from functions/fn_plot_parameter_samples.R
# pullSamples from functions/fn_pull_samples.R

pdf('Appendix_Figure4.pdf', width = 11, height = 7.5)
par(mfrow = c(2, 4)) 
plotParameterSamples(validationName = 'ancestry', genQuantName = 'omega_ancestry_new', points = T)
plotParameterSamples(validationName = 'temperature', genQuantName = 'alpha_climate_new', points = T)
plotParameterSamples(validationName = 'temperature', genQuantName = 'b_climate_new', points = T)
plotParameterSamples(validationName = 'temperature', genQuantName = 'EIR_climate_new', points = T)
plotParameterSamples(validationName = 'temperature', genQuantName = 'lf_climate_new', points = T)
plotParameterSamples(validationName = 'ancestry', genQuantName = 'pMI_ancestry_new', points = T)
plotParameterSamples(validationName = 'temperature', genQuantName = 'pMI_climate_new', points = T)
dev.off()