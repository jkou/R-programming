pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        count <- 0
        total <- 0
        
        for (i in id) {
             path <- ""
        	 ## Get path
        	 fileName <- formatC(i, width = 3, format = "d", flag = "0")
        	 path <- paste(directory, fileName, sep = "/")
        	 path <- paste(path, "csv", sep = ".")
            
          f <- read.csv(path)
          
          col <- f[pollutant]
          col <- na.omit(col)
          
          for (m in col[, 1]) {
              
                total <- total + m
                count <- count + 1
                
          }
          
       }
        
        round(total / count, digits = 3)
}
