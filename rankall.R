rankall <- function(outcome, rank = "best") {
  # First I will extract the data and subset it in order to have only the hospital names,
  # the states and the values for the desired outcome. At this stage I will check that the
  # outcome is valid and that the rank isn't an invalid string.
  # Then I will tapply a sorting function to all states.
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Return hospital name in that state with lowest 30-day death rate
  statedata <- data[data[,7] == state,]
  
  # First I need to extract the data for a particular outcome.
  # If the outome is invalid, I stop the execution of the function.
  if(outcome == "heart attack") {
    statedata[,11] <- as.numeric(statedata[,11])
    outcomeData <- statedata[!is.na(statedata[,11]),c(2,11)]
  }
  else if (outcome == "heart failure") {
    statedata[,17] <- as.numeric(statedata[,17])
    outcomeData <- statedata[!is.na(statedata[,17]),c(2,17)]
  }
  else if (outcome == "pneumonia") {
    statedata[,23] <- as.numeric(statedata[,23])
    outcomeData <- statedata[!is.na(statedata[,23]),c(2,23)]
  }
  else {
    stop("invalid outcome")
  }
  
  # Then I convert rank to a numeric value
  if (toupper(rank) == "BEST") {
    rank <- 1
  } else if (toupper(rank) == "WORST") {
    rank <- nrow(outcomeData)
  } else if (!is.numeric(rank)) {
    return(NA)
  }
  
  # Then I need to check if the rank is valid, otherwise I return a NULL
  if (nrow(outcomeData) < rank) {
    return(NA)
  }
  
  # In the end, I extract the data and return it
  outcomeData[,2] <- as.numeric(outcomeData[,2])
  outcomeData <- outcomeData[order(outcomeData[,2]),]
  finalData <- outcomeData[outcomeData[,2] == outcomeData[rank,2],]
  finalData <- finalData[order(finalData[,1]),]
  finalData[1,1]
}