# Appendix Fig. 3: Trace plots -----------------------------------

# requires the following functions:
# params from functions/fn_list_params.R

# create clearer labels
my_labels <- gsub('_ancestry_|_climate_', ', ', params)

# plot
plot_list <- list() 

for(i in seq_along(params)){
  plot_list[[i]] <- rstan::traceplot(r0_mod, par = params[i]) + ggtitle(my_labels[i]) + ylab('') + theme(legend.position="none") + theme(plot.title = element_text(size = 12, hjust = 0.5))
}

tracePlots <- grid.arrange(grobs=plot_list, ncol = 4)

ggsave('Appendix_Figure3.pdf', tracePlots, width = 8.5, height = 11)
