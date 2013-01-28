agecount <- function(age=NULL){
  homicides <- readLines("homicides.txt")
  if (is.null(age)){
    stop("value age is missing")
  }
  if (age <= 0){
    stop("age must be positive")
  }

  homicides <- readLines("homicides.txt")
  reg_exp <- paste(" ", age, " years old", sep="")
  length(grep(reg_exp, homicides))
}
