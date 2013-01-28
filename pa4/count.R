
count <- function(cause=NULL){
  
  causes <- c("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
  
  if (is.null(cause)){
    stop("parameter cause is missing")
  } 
  if (!(cause %in% causes)){
    stop("Wrong cause specified!")
  }
  
  homicides <- readLines("homicides.txt")
  
  # deal with the capitalization
  cause_w_cap = paste("[", toupper(substring(cause,1,1)), substring(cause,1,1), "]", substring(cause,2), sep="")
  
  reg_exp <- paste("<dd>[Cc]ause: ", cause_w_cap, "</dd>", sep="")
  length(grep(reg_exp, homicides))
}

