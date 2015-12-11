complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
	   		
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        arr <- data.frame(matrix(ncol = 2, nrow = 0))
        for (i in id) {
        	path = ""
        	 fileName <- formatC(i, width = 3, format = "d", flag = "0")
          path <- paste(directory, fileName, sep = "/")
          path <- paste(path, "csv", sep = ".") 
          
          f <- read.csv(path)
          
          ok <- complete.cases(f)
          newr <- data.frame(matrix(ncol = 2, nrow = 0))
          newr[1,1] <- i
          newr[1,2] <- sum(ok)
          arr <- rbind(arr, newr)   

        }
        
        names(arr) = c("id", "nobs")
        arr        
}
