overlay_distributions_plot <- function(mod, param_name, priorValue1, priorValue2){
  # get posterior distribution
  df <- as.data.frame(rstan::extract(mod, param_name))
  # plot
  paramTitle <- gsub('_climate_|_ancestry_', ', ', param_name)
  p <- ggplot(df, aes_string(param_name)) +
    geom_histogram(aes(y = after_stat(density)), color = 'black', fill =  'lightblue', alpha = 0.4) +
    theme_classic() +
    ggtitle(paramTitle) +
    ylab('') +
    xlab('')
  
  p  + 
    stat_function(
      fun = dnorm, 
      args = list(mean = priorValue1, sd = priorValue2), 
      lwd = 2, 
      col = 'black'
    )
  
}
