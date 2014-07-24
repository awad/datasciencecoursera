corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  result <- data.frame()
  res <- numeric()
  ## Return a numeric vector of correlations
  for(file in list.files(directory)){
    fullFilePath <- paste(directory, file, sep="/")
    
    data <- read.csv(fullFilePath)
    complete = sum(complete.cases(data))
    if(complete >threshold)
    {
      res <- c(res, cor(data$nitrate, data$sulfate, use = "complete.obs"))
      #result <- rbind(result, cor(data$nitrate, data$sulfate, use = "complete.obs"))
    }
  }
  res
}