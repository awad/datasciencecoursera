rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if(num == "best")
  {
    num <- 1
  }

  outcome<- simpleCap(outcome)
  
  #Convert "Heart Attack" to "Heart.Attack"
  outcome<-sub(" ", ".", outcome)
  
  #Concat
  rowName <- paste("Hospital.30.Day.Death..Mortality..Rates.from", outcome, sep=".");
  
  #Use Order Method to rank the hospitals first by rowName and then by Name
  res <- outcomeData[outcomeData$State == state, ][order(as.numeric(as.character(outcomeData[outcomeData$State == state, ][[rowName]])), outcomeData[outcomeData$State == state, ]$Hospital.Name, na.last = NA), ]
  
  if(num=="worst")
  {    
    num <- nrow(res)
  }
  
  #print(num)
  #print(res["Hospital.Name"])
  #res[c("Hospital.Name", rowName)][num]
  res$Hospital.Name[num]  
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}