best <- function(state, outcome)
{
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
  
  ## Return hospital name in that state with the lowest 30-day death 
  ## rate 
  data <- outcome_csv[outcome_csv$State == state,]
  data[, sc] <- sapply(data[, sc], as.numeric)
  data <- data[order(data[ , 2]), ]
  best <- data[which.min(data[ , sc]), "Hospital.Name"]
  best
}