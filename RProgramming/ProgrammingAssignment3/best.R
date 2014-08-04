best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  if(nrow(outcomeData[outcomeData$State == state, ]) < 1)
  {
    stop("invalid state")
  }
  
  if(outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia")
  {
    stop("invalid outcome")
  }
  
  ##Computing rowName from outcome
  #Convert "heart attack" to "Heart Attack"
  outcome<- simpleCap(outcome)
  
  #Convert "Heart Attack" to "Heart.Attack"
  outcome<-sub(" ", ".", outcome)
  
  #Concat
  rowName <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome, sep=".");
  #print(rowName)
  
  #Use Order Method to rank the hospitals first by rowName and then by Name
  outcomeData[outcomeData$State == state, ][order(as.numeric(as.character(outcomeData[outcomeData$State == state, ][[rowName]])), outcomeData[outcomeData$State == state, ]$Hospital.Name), ]$Hospital.Name[1]
  
  
  ##Scratch Pad
  #res <- outcomeData[outcomeData$State == state, ][order(as.numeric(as.character(outcomeData[outcomeData$State == state, ][[rowName]])), outcomeData[outcomeData$State == state, ]$Hospital.Name), ]
  #rowNames <- c("Hospital.Name", rowName)
  #print(rowNames)
  #res[, rowNames]
  #data <- outcomeData[outcomeData$State == state, ][[rowName]]
  #data <- as.numeric(data)
  #print(sort(data)[1])
  #order
  #minRate <- min(data, na.rm = TRUE)
  #print(minRate)
  #hospitalNames <- outcomeData[outcomeData[outcomeData$State == state, ][[rowName]] == minRate, ]$Hospital.Name
  #outcomeData[outcomeData[outcomeData$State == state, ][[rowName]] == minRate, ][[rowName]]

  #print(sort(hospitalNames)[1])
  #res <- sort(hospitalNames)[1]
  #resData <- outcomeData[outcomeData$Hospital.Name == res, ][[rowName]]
  #resData
    
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}