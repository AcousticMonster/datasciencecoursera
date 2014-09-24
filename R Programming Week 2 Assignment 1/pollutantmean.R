## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)

## Written by: Danny Korves 9/12/2014

pollutantmean <- function(directory, pollutant, id = 1:332) {

              
        ## create a list of files (based on the directory variable)
        ## base directory = "specdata"
        poll_files_list <- list.files(directory, full.names=TRUE)
        
        ## creates an empty data frame
        poll_dat <- data.frame() 
        
        ## loops through the files, rbinding them together
        for (i in id) { 
                poll_dat <- rbind(poll_dat, read.csv(poll_files_list[i]))
        }
        
        ## calculate mean for imputed pollutant
        ## either "sulfate" or "nitrate"
        mean(poll_dat[, pollutant], na.rm=TRUE) 

}
