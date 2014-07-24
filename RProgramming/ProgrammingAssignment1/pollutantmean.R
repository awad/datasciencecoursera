pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  filename <-paste(sprintf("%03d", id), "csv", sep=".")
  files <- paste(directory, filename, sep="/")
  mydata <- data.frame()
  for(file in files){
  mydata <- rbind(mydata, read.csv(file))
  
  #print(mydata[pollutant])
  
  }
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  #colMeans(mydata[pollutant],na.rm=TRUE)
  mean(as.matrix(mydata[pollutant]), na.rm=TRUE)

}