best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia)
                              stringsAsFactors = FALSE) 
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% fd[, "state"]){
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else {
    validrowIndices <- which(fd[, "state"] == state)
    validrows <- fd[validrowIndices, ]    # extracting data for the called state
    mortality <- suppressWarnings(as.numeric(validrows[, eval(outcome)]))
    min_mortality <- min(mortality, na.rm = TRUE)
    bestHospList  <- validrows[which(mortality == min_mortality), "hospital"]
    bestHosp  <- bestHospList[order(bestHospList)]
  }
  return(bestHosp)
  
}
