rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

#-------------------------------------------------------------

  source("rankhospital.R")
  
  dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  dataset[,outcome_list[outcome]] <- as.numeric(dataset[,outcome_list[outcome]])

  #check the state input
  states_list <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY",
                   "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                   "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
  
  best_hospitals <- c()
  states <- c()
  for (state in states_list){
    #ATTENTION!!! rankhospital changed in the file!!!!!
    hospital <- rankhospital(state, outcome, num, dataset)
    #ATTENTION!!! 
    best_hospitals <- append(best_hospitals, hospital)
    states <- append(states, state)
  }
  

  output <- data.frame(hospital=best_hospitals, state=states)
  
  
  
}

