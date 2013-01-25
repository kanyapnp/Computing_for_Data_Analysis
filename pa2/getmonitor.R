getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE
        
        ## Your code here
	if( id>332 || id<0){
		print("Please input a correct id");
		return("ERROR");
	}
	while (nchar(id)<3){
		id <- paste('0', id, sep='')
	}
	filepath <- paste(directory, '/', id, ".csv",sep = '');
	file <- read.csv(filepath);

	if(summarize == TRUE) {
		summary(file);
	}
	return(file);
}



