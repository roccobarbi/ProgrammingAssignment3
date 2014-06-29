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
    result <- finalData[1,c(1,3)]
    return(result)
    if(exists("nobsdf")) {
      nobsdf[ki,] <- c(id[ki], nrow(currentstation[complete.cases(currentstation),]))
    }
    else {
      nobsdf <- data.frame(id = id[ki], nobs = nrow(currentstation[complete.cases(currentstation),]))
    }   
  }
  
  # Function to extract and return my data
  sortData <- function(outcomeData){
    outcomeData[,2] <- as.numeric(outcomeData[,2])
    outcomeData <- outcomeData[order(outcomeData[,2]),]
    finalData <- outcomeData[outcomeData[,2] == outcomeData[rank,2],]
    finalData <- finalData[order(finalData[,1]),]
    finalData[1,1]
  }
  
  # And now I tapply it and assign it to a data frame
  
  result <- tapply(outcomeData[,1:2],outcomeData[,3],sortData)
  result
}