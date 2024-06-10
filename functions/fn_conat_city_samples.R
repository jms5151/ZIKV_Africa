concatCitySamples <- function(validationName, genQuantName){
  # get data
  indexes <- which(mod_data$validationtype == validationName)
  samps <- list_of_draws[[genQuantName]][, indexes]
  x <- as.data.frame(t(samps))
  x$City <- mod_data$location[mod_data$validationtype == validationName]
  x$year <- mod_data$year[mod_data$validationtype == validationName]
  x$lon <- mod_data$lon[mod_data$validationtype == validationName]
  x$lat <- mod_data$lat[mod_data$validationtype == validationName]
  
  # Reshape data to long format
  long_df <- x %>%
    pivot_longer(cols = -c(City, year, lon, lat), names_to = "observation", values_to = "value")
  
  result <- long_df %>%
    group_by(City, year, lat, lon) %>%
    summarize(median_value = median(value, na.rm = TRUE)) %>%
    as.data.frame()
  
  return(result)  
}

