rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Slice data and Rename Columns
        sliced_od <- outcomedata[c(2, 7, 11, 17, 23)]
        names(sliced_od) <- c("hospitalname", "state", "heart attack", "heart failure", "pneumonia")
        
        ## ------------------------------------------------------------------

        ## Validate the outcome string
        ## 1. Test the outcome input (stop if invalid)
        check_outcome <- c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% check_outcome == FALSE ) stop("invalid outcome")
        
        ## 2. Test the num input (stop if invalid)
        if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
        
        ## ------------------------------------------------------------------
        
        ## Select only rows with valid data
        sliced_od <- sliced_od[sliced_od[outcome] != 'Not Available', ]
                
        ## Order the outcomedata
        sliced_od[outcome] <- as.data.frame(sapply(sliced_od[outcome], as.numeric))
        sliced_od <- sliced_od[order(sliced_od$hospitalname, decreasing = FALSE), ]
        sliced_od <- sliced_od[order(sliced_od[outcome], decreasing = FALSE), ]
                
        
        ## sub function to process the num argument
        HospByRank <- function(df, s, n) {
                df <- df[df$state==s, ]
                vals <- df[, outcome]
                if( n == "best" ) {
                        rowNum <- which.min(vals)
                } else if( n == "worst" ) {
                        rowNum <- which.max(vals)
                } else {
                        rowNum <- n
                }
                df[rowNum, ]$hospitalname
        }

        
        ## For each state, find the hospital of the given rank
        states <- sliced_od[, 2]
        states <- unique(states)
        new_sliced_od <- data.frame("hospital"=character(), "state"=character())
        for(st in states) {
                hosp <- HospByRank(sliced_od, st, num)
                new_sliced_od <- rbind(new_sliced_od, data.frame(hospital=hosp, state=st))
        }

        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        new_sliced_od <- new_sliced_od[order(new_sliced_od['state'], decreasing = FALSE), ]
        
        new_sliced_od
}

