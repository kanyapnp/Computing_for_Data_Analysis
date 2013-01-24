rankhospital <- function(state, outcome, num="best"){
# state <- "TX"
# outcome <- "heart failure"
# num <- "5"
# 

#initialize the outcome
outcome_list <- c(11,17,23)
names(outcome_list) <- c("heart attack", "heart failure", "pneumonia")

#the collumn of name
name_col <- 2

#check outcome input
if (!(outcome %in% names(outcome_list))){
  stop("invalid outcome")
}

#set the outcome to the column
col_id = outcome_list[outcome]


#check the state input
states_list <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY",
                 "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK",
                 "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
if (!(state %in% states_list)){
  stop("invalid state")
}


#--------------------read and process the data-------------------------

#read the data
outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# outcome.all <- dataset
#convert character to numeric

outcome.all[,outcome_list[outcome]] <- as.numeric(outcome.all[,outcome_list[outcome]])


#filter the data by state
check_state <- outcome.all$State == state
outcome.state <- outcome.all[check_state,]

#clean the data
is_na <- is.na(outcome.state[,col_id])
outcome.state <- outcome.state[!is_na,]

#the number of hospitals in the state
num_hospitals <- nrow(outcome.state)

#check the num input
if (num == "best"){
  num <- 1
}
if (num == "worst"){
  num = num_hospitals
}
if (num <1 || num > num_hospitals){
  return(NA)
}

#order the hospitals by outcome
outcome.order <- outcome.state[with(outcome.state, order(outcome.state[,col_id],
                                                         outcome.state[,name_col])),]

best_hospital <- outcome.order[as.numeric(num),2]

}


