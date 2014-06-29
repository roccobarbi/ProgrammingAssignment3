rankall <- function(outcome, rank = "best") {
  # First I will extract the data and subset it in order to have only the hospital names,
  # the states and the values for the desired outcome. At this stage I will check that the
  # outcome is valid and that the rank isn't an invalid string.
  # Then I will tapply a sorting function to all states.
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # First I need to extract the data for a particular outcome.
  # If the outome is invalid, I stop the execution of the function.
  if(outcome == "heart attack") {
    data[,11] <- as.numeric(data[,11])
    outcomeData <- data[!is.na(data[,11]),c(2,11,7)]
  }
  else if (outcome == "heart failure") {
    data[,17] <- as.numeric(data[,17])
    outcomeData <- data[!is.na(data[,17]),c(2,17,7)]
  }
  else if (outcome == "pneumonia") {
    data[,23] <- as.numeric(data[,23])
    outcomeData <- data[!is.na(data[,23]),c(2,23,7)]
  }
  else {
    stop("invalid outcome")
  }
  
  # Then I convert rank to a numeric value
  if (toupper(rank) == "BEST") {
    rank <- 1
  } else if (!is.numeric(rank)) {
    return(NA)
  }
  
  splitData <- split(outcomeData,outcomeData[,3])
  
  for(ki in seq_along(names(splitData))) {
    stateList <- names(splitData)
    currentState <- stateList[ki]
    stateData <- as.data.frame(splitData[currentState])
    colnames(stateData) <- c("Hospital","Outcome","State")
    stateData[,2] <- as.numeric(stateData[,2])
    stateData <- stateData[order(stateData[,2]),]
    finalData <- stateData[stateData[,2] == stateData[rank,2],]
    finalData <- finalData[order(finalData[,1]),]
    # result <- as.vector(finalData[1,c(1,3)])
    result <- c(as.character(finalData[1,1]),as.character(finalData[1,3]))
    if(exists("finalResult")) {
      finalResult[ki,"Hospital"] <- result[1]
      finalResult[ki,"State"] <- result[2]
    }
    else {
      finalResult <- data.frame(Hospital = result[1], State = result[2], stringsAsFactors=FALSE)
    }   
  }
  return(finalResult)
}