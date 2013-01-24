file <- "outcome-of-care-measures.csv"
outcome <- read.csv(file, colClasses = "character")

cols_id <- c(11,17,23)


for(i in cols_id){
    outcome[,i] = as.numeric(outcome[,i])
}


hist_generator <- function(){
  # format the plot
  par(mfrow = c(1,3))
  titles <- c("Heart Attack", "Heart Failure", "Pneumonia")
  
  # multiple plots
  for (i in 1:3){
    
    #compute the mean and sd of the column
    u = mean(outcome[,cols_id[i]], na.rm=TRUE)
    sd = sqrt(var(outcome[,cols_id[i]], na.rm=TRUE))
    
    # format the title
    title = substitute(title * "(" * bar(x) * " = " * mean * ")", list(title = titles[i], mean = u))
    
    #make histogram
    hist(outcome[,cols_id[i]], main=title, xlab="30-day Death Rate", xlim=c(5,25), prob=TRUE)
  
    #add a line indicating the median value
    median <- median(outcome[,cols_id[i]], na.rm= TRUE)
    abline(v = median)
    
    #add the density approximation, using normal distribution
    x <- seq(5,25,0.01)
    lines(x, dnorm(x, mean=u, sd=sd))
  }   
}


# hist_generator()

box_generator <- function(){
  
  # get the list containing states with at least 20 hospitals
  state_table <- table(outcome$State)
  more_than_20 <- state_table >= 20
  states_20_plus <- names(state_table[more_than_20])
  
  # remove the states with less than 20 hospitals from the dataset
  on_list <- outcome$State %in% states_20_plus
  outcome2 <- outcome[on_list,]
  
  death <- outcome2[,11]
  state <- outcome2$State
  par(mfrow=c(1,1), las=2 )
#   boxplot(death ~ state, data = median_death,main="Heart Attack 30-day Death Rate by State", ylab="30-day Death Rate by State")
  
  # split the data into states
  death_in_state <- split(outcome2[,11], outcome2$State)
  
  # compute the median of deathes in every state
  median_death <- sapply(death_in_state, mean, na.rm=TRUE)
  
  # sort the list
  median_death <- sort(median_death)
  
  state_names <- names(median_death)
  
  #modify the factor using sorted levels
  state <- factor(state, state_names)
  
  
  # add the modified x-axis
  names_w_num <- c()
  for(i in state_names){
    state_num = paste(i,'(',state_table[i],')',sep='')
    names_w_num = append(names_w_num, state_num)
  }
  par(cex.axis=1, las=2)
  boxplot(death ~ state, main="Heart Attack 30-day Death Rate by State", ylab="30-day Death Rate by State", xaxt="n")
  axis(1, at=1:length(state_names), labels = names_w_num, cex.axis=0.7)
  

}

library(lattice)

xy_generator(){
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv("hospital-data.csv", colClasses = "character")
  
  outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
  npatient <- as.numeric(outcome.hospital[, 15])
  owner <- factor(outcome.hospital$Hospital.Ownership)
  
  xyplot(death ~ npatient|owner,  xlab="Number of Patients Seen", ylab="30-day Death Rate",
         main="Heart Attack 30-day Death Rate by Ownership", panel = function(x,y,...){
           panel.xyplot(x,y)
           panel.lmline(x,y)
           }  )
}


