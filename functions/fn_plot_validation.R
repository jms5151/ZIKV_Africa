validation_plot <- function(df){
  p <- df %>%
    ggplot(aes(x = R0_median, y = S_median, group = Model)) +
    geom_errorbar(aes(ymin = S_5th, ymax = S_95th, col = Country)) +
    geom_errorbarh(aes(xmin = R0_5th, xmax = R0_95th, col = Country)) +
    geom_point(aes(x = R0_median, y = S_median), shape = 21) +
    geom_smooth(method = 'lm', se = TRUE, color = 'black') +
    stat_cor(aes(label = after_stat(rr.label)), vjust = 0.5, hjust = 0.4) +
    facet_grid( ~Model, scales = 'free_x') +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    ylab('Seroprevalence (%)') +
    xlab(expression(paste('Model estimated ', R[0]))) +
    geom_vline(xintercept = 1, linetype = 'dashed', linewidth = 0.75) +
    theme(legend.position = 'bottom') +
    stat_ellipse(level = 0.95, col = 'darkgrey', linetype = 'dashed') +
    coord_cartesian(ylim = c(0, 30))

  return(p)
  
}

