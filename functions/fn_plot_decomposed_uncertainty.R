plotUncertaintyDecomposed <- function(df){
  dodge <- position_dodge(width = 0.8)
  
  p <- ggplot(df, aes(x = Point_mean, y = City, color = Uncertainty_Type)) +
    geom_point(size = 3, position = dodge) + 
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, position = dodge) +  # Horizontal error bars with dodge
    labs(x = expression(paste('Mean ', R[0])), y = "City", title = "Uncertainty decomposition by city") +
    theme_minimal() +
    labs(color = "Source of Uncertainty") +
    scale_color_manual(
      values = c('Param_uncertainty' = 'darkred', 'Climate_aaa_uncertainty' = 'blue', 'Total_uncertainty' = 'black'),
      labels = c('Param_uncertainty' = 'Parameter', 'Climate_aaa_uncertainty' = 'Climate & %Aaa', 'Total_uncertainty' = 'Total')
    )
  
  return(p)  
}
