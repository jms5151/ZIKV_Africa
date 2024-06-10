r0_plot <- function(validationName, genQuantName) {
  # get samples
  x <- pullSamples(validationName = validationName, genQuantName = genQuantName)
  # get data
  indexes <- which(mod_data$validationtype == validationName)
  if(grepl('climate', genQuantName) == TRUE){
    xvals <- mod_data$temp_new[indexes]
    xLabel <- expression(paste("Temperature (",degree,"C)"))
    pointDataXColName <- gsub('_new', '_temp', genQuantName)
  } else if(grepl('ancestry', genQuantName) == TRUE){
    xvals <- mod_data$aa_new[indexes] * 100
    xLabel <- 'Human specialist ancestry\n(%Aaa)'
    pointDataXColName <- gsub('_new', '_aa', genQuantName)
  }
  p <- ggplot() +
    geom_line(aes(xvals, x[,2])) +
    geom_line(aes(xvals, x[,1]), col = 'red', linetype = 'dashed') +
    geom_line(aes(xvals, x[,3]), col = 'red', linetype = 'dashed') +
    theme_classic() +
    theme(text = element_text(size = 16)) +
    ylab(expression(R[0])) +
    xlab(xLabel)
  
  return(p)
}