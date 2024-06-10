# Appendix Fig. 3: Trace plots -----------------------------------

# list parameters
params <- c('omega_ancestry_constant'
            , 'omega_ancestry_d'
            , 'omega_ancestry_e'
            , 'alpha_climate_Tmin'
            , 'alpha_climate_Tmax'
            , 'alpha_climate_constant'
            , 'b_climate_Tmin'
            , 'b_climate_Tmax'
            , 'b_climate_constant'
            , 'EIR_climate_Tmin'
            , 'EIR_climate_Tmax'
            , 'EIR_climate_constant'
            , 'lf_climate_Tmin'
            , 'lf_climate_Tmax'
            , 'lf_climate_constant' 
            , 'pMI_ancestry_constant'
            , 'pMI_ancestry_d'
            , 'pMI_ancestry_e'
            , 'pMI_climate_rmax'
            , 'pMI_climate_Topt'
            , 'pMI_climate_a'
)

# create clearer labels
my_labels <- gsub('_ancestry_|_climate_', ', ', params)

# plot
plot_list <- list() 

for(i in seq_along(params)){
  plot_list[[i]] <- rstan::traceplot(r0_mod, par = params[i]) + ggtitle(my_labels[i]) + ylab('') + theme(legend.position="none") + theme(plot.title = element_text(size = 12, hjust = 0.5))
}

tracePlots <- grid.arrange(grobs=plot_list, ncol = 4)

ggsave('Appendix_Figure3.pdf', tracePlots, width = 8.5, height = 11)
