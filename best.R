best <- function(state, outcome) {
  # This function returns the best hospital (lower mortality rate)
  # for a given outcome
  state <- toupper(state)
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that the states and outcome are valid
  if(!(state %in% data[,7])) {
    stop("invalid state")
  }
  
  # Return hospital name in that state with lowest 30-day death rate
  statedata <- data[data[,7] == state,]
  if(outcome == "heart attack") {
    # heart attack
    statedata[,11] <- as.numeric(statedata[,11])
    finalData <- statedata[!is.na(statedata[,11]),c(2,11)]
  }
  else if (outcome == "heart failure") {
    # heart failure
    statedata[,17] <- as.numeric(statedata[,17])
    finalData <- statedata[!is.na(statedata[,17]),c(2,17)]
  }
  else if (outcome == "pneumonia") {
    # pneumonia
    statedata[,23] <- as.numeric(statedata[,23])
    finalData <- statedata[!is.na(statedata[,23]),c(2,23)]
  }
  else {
    stop("invalid outcome")
  }
  finalData[,2] <- as.numeric(finalData[,2])
  finalData <- finalData[finalData[,2] == min(finalData[,2]),]
  finalData[order(finalData[,2]),]
  finalData[1,1]
#  finalData[,2] == min(finalData[,2])
}