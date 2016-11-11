best <- function(state, outcome) {
      ## Read outcome data
      data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      data[, 11] <- as.numeric(data[, 11])
      data[, 17] <- as.numeric(data[, 17])
      data[, 23] <- as.numeric(data[, 23])
      
      ## Check that state and outcome are valid
      if(sum(data[, 7] == state) == 0) stop("invalid state")
      
      conditions <- c("heart attack", "heart failure", "pneumonia")
      if(sum(conditions == outcome) == 0) stop("invalid outcome")
      
      ## Return hospital name in that state with lowest 30-day death rate
      state
      
      
}