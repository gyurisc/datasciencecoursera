rankall <- function(outcome, num = "best") {
  
  rankhospital_state <- function(state, outcome) {
    
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

  
  ## Read outcome data
  outcome_csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  ## Check that state and outcome are valid 
  Outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(!outcome %in% Outcomes)
    stop("invalid outcome")
  
  ## For each state, find the hospital of the given rank 
  states <- unique(outcome_csv$State)
  states <- sort(states)

  ranks <- vector(mode="character", length=length(states))
  for(i in 1:length(states))
  {
    state <- states[i]
    ranks[i] <- rankhospital_state(state, outcome)
  }

  ## Return a data frame with the hospital names and the 
  ## (abbreviated) state name
  
  data.frame(hospital = ranks, state = states)
}