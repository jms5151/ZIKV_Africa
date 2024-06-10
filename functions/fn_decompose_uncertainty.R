decomposeUncertainty <- function(validationName, genQuantName){
  # get data
  indexes <- which(mod_data$validationtype == validationName)
  samps <- list_of_draws[[genQuantName]][, indexes]
  
  sampMeans <- colMeans(samps, na.rm = T)
  sampVar <- colVars(samps, na.rm = T)
  
  x <- as.data.frame(cbind(sampMeans, sampVar)) 
  x$City <- mod_data$location[mod_data$validationtype == validationName]
  x$year <- mod_data$year[mod_data$validationtype == validationName]
  x$model <- mod_data$model[mod_data$validationtype == validationName]
  
  x <- subset(x, model != 'WorldClim_Historical')
  
  x2 <- x %>%
    group_by(City) %>%
    summarise('Point_mean' = mean(sampMeans),
              'Param_uncertainty' = var(sampMeans),
              'Climate_aaa_uncertainty' = mean(sampVar))
  
  x2$Total_uncertainty <- rowSums(x2[,c('Param_uncertainty', 'Climate_aaa_uncertainty')])
  
  return(x2)  
}