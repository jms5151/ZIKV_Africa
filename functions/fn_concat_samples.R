concatAncestrySamples <- function(validationName, genQuantName, percentiles){
  # get data
  x <- pullSamples(validationName = validationName, genQuantName = genQuantName, percentiles)
  x <- as.data.frame(x)
  colnames(x) <- c('lower', 'median', 'upper')
  indexes <- which(mod_data$validationtype == validationName)
  x$anc <- mod_data$aa_new[indexes]
  x$site = mod_data$location[indexes]
  x$year = mod_data$year[indexes]
  x$temp = mod_data$temp_new[indexes]
  x$lat = mod_data$lat[indexes]
  x$lon = mod_data$lon[indexes]
  return(x)  
}