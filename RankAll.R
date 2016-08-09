rankall <- function(outcome, rank = "best") {
  
  outcomeCSV <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd <- as.data.frame(cbind(outcomeCSV[,2], outcomeCSV[,7], outcomeCSV[,11], outcomeCSV[,17], outcomeCSV[,23]), stringsAsFactors = FALSE)
  dfCurrentItr <- data.frame()
  resultdf <- data.frame()
  
  colnames(fd) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  if(!(eval(outcome) %in% colnames(df))) print("Invalid outcome")
  states <- unique(fd[,"state"])
  for(i in 1:length(states)){
    validrowIndices <- which(fd[, "state"] == eval(states[i]))
    validrows <- fd[validrowIndices, ] 
    validrows[, eval(outcome)] <- suppressWarnings(as.numeric(validrows[, eval(outcome)]))
    validrows <- validrows[order(validrows[,eval(outcome)],validrows[,"hospital"], decreasing = FALSE), ]
    if(eval(rank) == "best"){
      min_mortality <- min(validrows[, eval(outcome)], na.rm = TRUE)
      dfCurrentItr <- as.data.frame(cbind(validrows[which(validrows[, eval(outcome)]==eval(min_mortality)), "hospital"], validrows[which(validrows[, eval(outcome)]==eval(min_mortality)),"state"]), stringsAsFactors = FALSE)
    }else if(eval(rank) == "worst"){
        max_mortality <- max(validrows[, eval(outcome)], na.rm = TRUE)
        dfCurrentItr <- as.data.frame(cbind(validrows[which(validrows[, eval(outcome)]==eval(max_mortality)), "hospital"], validrows[which(validrows[, eval(outcome)]==eval(max_mortality)),"state"]), stringsAsFactors = FALSE)
    }else{
      if(eval(rank) > nrow(validrows)){
        dfCurrentItr <- as.data.frame(cbind("NA", states[i]), stringsAsFactors = FALSE)
      }else{
      validrows <- subset(validrows, !is.na(validrows[, eval(outcome)]))  
      dfCurrentItr <- as.data.frame(cbind(validrows[as.numeric(eval(rank)), "hospital"], validrows[as.numeric(eval(rank)),"state"]), stringsAsFactors = FALSE)
      }
    }
   resultdf <- rbind(resultdf,dfCurrentItr)
    
  }
  colnames(resultdf) <- c("hospital","state")
  resultdf <- resultdf[order(resultdf[,"state"], decreasing = FALSE), ]
  return(resultdf)
  
}  