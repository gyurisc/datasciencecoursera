rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcome_csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  StateFilter = outcome_csv[, "State"] == state
  State = outcome_csv[StateFilter,]
  Outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid 
  if(nrow(State) == 0)
    stop("invalid state")
  
  if(!outcome %in% Outcomes)
    stop("invalid outcome")
  
  if(outcome == "heart attack") { 
    sc <- 11 
  }
  
  if(outcome == "heart failure") { 
    sc <- 17 
  }
  
  if(outcome == "pneumonia") { 
    sc <- 23 
  }
  
  ## Return hospital name in that state with the given rank 30 day death rate
  data <- outcome_csv[outcome_csv$State == state, c(2, sc)]
  data[, 2] <- sapply(data[, 2], as.numeric)
  ordered_data <- order(data[, 2], data$Hospital.Name, na.last = NA)
  
  if(num == "best") {
    as.character(data$Hospital.Name[ordered_data[1]])
  } else if (num == "worst") {
    as.character(data$Hospital.Name[ordered_data[length(ordered_data)]])
  } else 
  {
    as.character(data$Hospital.Name[ordered_data[num]])
  }
}