
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

complete <- function(directory, id = 1:332) {

        ## create a list of files (based on the directory variable)
        ## base directory = "specdata"
        comp_files_list <- list.files(directory, full.names=TRUE)
        
        ## creates an empty data frame
        comp_dat <- data.frame() 
               
        ## loops through the files, rbinding them together
        for (i in id) { 
               
               ## get the csv data from current file name in loop
               data <- read.csv(comp_files_list[i])
               
               ## sums complete cases for current csv and adds row to comp_dat
               comp_dat <- rbind(comp_dat, c(i, sum(complete.cases(data))))                                       
                               
        }
        
        ## add column names to the two elements
        colnames(comp_dat) <- c("id", "nobs")
        
        comp_dat

}
