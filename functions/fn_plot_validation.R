validation_plot <- function(df){
  p <- df %>%
    ggplot(aes(x = S_median, y = R0_median, group = Model)) +
    geom_errorbarh(aes(xmin = S_5th, xmax = S_95th, col = Country)) +
    geom_errorbar(aes(ymax = R0_95th, ymin = R0_5th, col = Country)) +
    geom_point(aes(x = S_median, y = R0_median)) +
    geom_smooth(method = 'lm', se = TRUE, color = 'black', fullrange = TRUE) +
    stat_cor(aes(label = after_stat(rr.label)), vjust = -3) +
    facet_grid( ~Model) +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    xlab('Seroprevalence') +
    ylab(expression(paste('Model estimated  ', R[0]))) +
    theme(legend.position = 'bottom') #+
  guides(color = 'none')
  
  return(p)
  
}

