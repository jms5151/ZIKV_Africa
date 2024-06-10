plotParameterSamples <- function(validationName, genQuantName, points){
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
    xLabel <- 'Human specialist ancestry (% Aaa)'
    pointDataXColName <- gsub('_new', '_aa', genQuantName)
    mod_data[[pointDataXColName]] <- mod_data[[pointDataXColName]] * 100
  }
  pointDataYColName <- gsub('_new', '', genQuantName)
  # set plotting conditions
  yMax = max(x[,3])
  # plot
  titleName <- gsub('_.*', '', genQuantName)
  plot(xvals, x[,2], type = 'l', lwd = 2, ylab = '', xlab = xLabel, ylim = c(0, yMax), main = titleName, las = 1)
  lines(xvals, x[,1], lty=2, col='red', ylim=c(0,yMax))
  lines(xvals, x[,3], lty=2, col='red', ylim=c(0,yMax))
  # add points
  if(points == TRUE){
    points(mod_data[[pointDataXColName]], mod_data[[pointDataYColName]], pch=16, ylim=c(0,yMax))
  }
}