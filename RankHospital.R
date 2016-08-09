rankhospital <- function(state, outcome, rank="best"){
  
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd <- as.data.frame(cbind(outcomeCSV[,2], outcomeCSV[,7], outcomeCSV[,11], outcomeCSV[,17], outcomeCSV[,23]), stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital","state","heart attack","heart failure","pneumonia")
  if(!(eval(state) %in% fd[,"state"])) print("Invalid state")
  else if(!(eval(outcome) %in% colnames(fd))) print("Invalid outcome")
  validrowIndices <- which(fd[, "state"] == state)
  validrows <- fd[validrowIndices, ]    # extracting data for the called state
  validrows[, eval(outcome)] <- suppressWarnings(as.numeric(validrows[, eval(outcome)]))
  
  ## To get the Best Hospital
  if(eval(rank)=="best"){
    best(state,outcome)
  ## To get the worst Hospital  
  }else if(eval(rank)=="worst"){
    validrows <- validrows[order(validrows[,eval(outcome)],validrows[,"hospital"], decreasing = TRUE), ]
    worstHosp <- validrows[, "hospital"][1]
    return(worstHosp)   
  }else{
    validrows <- validrows[order(validrows[,eval(outcome)],validrows[,"hospital"], decreasing = FALSE), ]
    rankedHospital <- validrows[, "hospital"][as.numeric(eval(rank))]
    return(rankedHospital)
  }
  
}