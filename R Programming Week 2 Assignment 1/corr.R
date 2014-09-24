## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlationsk


corr <- function(directory, threshold = 0) {
        
        ## create a list of files (based on the directory variable)
        ## base directory = "specdata"
        corr_files_list <- list.files(directory, full.names=TRUE)        
        
        ## create numeric vector to store correlation data
        corrr <- numeric()
        
        for(i in 1:332) {
                ## Read the CSV for current file in loop
                data <- read.csv(corr_files_list[i])
                
                ## sum the complete cases for comparison to threshold
                comp_observ <- sum(complete.cases(data))
                
                ## collect the records that have no NA values
                data_clean <- na.omit(data)
                
                ## loop thru each case to see if sum >= threshold
                if (comp_observ >= threshold) {
                        
                        ## if true correlate "sulfate" and "nitrate"
                        corrr[i] <- cor(data_clean$sulfate, data_clean$nitrate)
                }
                else {
                        ## if false add NA
                        corrr[i] <- NA   
                }
        }
        
        ## remove the NA elements
        clean_corrr <- na.omit(corrr)
        clean_corrr
}



