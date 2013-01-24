# given state and outcome, find the best hospital dealing the outcome in the state

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  #check the outcome
  outcome_list <- c(11,17,23)
  names(outcome_list) <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% names(outcome_list))){
    stop("invalid outcome")
  }
  
  col_id = outcome_list[outcome]
  
  
  #check the state
  states_list <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY",
                   "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                   "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  if (!(state %in% states_list)){
    stop("invalid state")
  }
  
  #read the file
  outcome.all = read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  #convert to numeric
  for (i in outcome_list){
    outcome.all[,i] <- as.numeric(outcome.all[,i])
  }
  
  #filter the data by state
  check_state <- outcome.all$State == state
  outcome.state <- outcome.all[check_state,]
  
  # the col id of hospital name
  name_col <- 2
  outcome.order <- outcome.state[with(outcome.state, order(outcome.state[,col_id],
                                                           outcome.state[,name_col])),]
  best_name <- outcome.order[1,2]
  print(as.character(best_name))
}







