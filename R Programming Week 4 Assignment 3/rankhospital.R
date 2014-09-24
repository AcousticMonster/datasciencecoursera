rankhospital <- function(state, outcome, num) {
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
        
        ## 2. Test the state input (stop if invalid)
        states <- sliced_od[, 2]
        states <- unique(states)
        if( state %in% states == FALSE ) stop("invalid state")
                
        ## 3. Test the num input (stop if invalid)
        if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
        
        ## ------------------------------------------------------------------
        
        ## Select rows based on inputed state value
        sliced_od <- sliced_od[sliced_od$state == state & sliced_od[outcome] != 'Not Available', ]
        
        ## Order the sliced_od
        sliced_od[outcome] <- as.data.frame(sapply(sliced_od[outcome], as.numeric))
        sliced_od <- sliced_od[order(sliced_od$hospitalname, decreasing = FALSE), ]
        sliced_od <- sliced_od[order(sliced_od[outcome], decreasing = FALSE), ]
        
        ## ------------------------------------------------------------------
        
        
        ## Choose ranking results based on the inputed "num" value
        vals <- sliced_od[, outcome]
        if( num == "best" ) {
                rowNum <- which.min(vals)
        } else if( num == "worst" ) {
                rowNum <- which.max(vals)
        } else {
                rowNum <- num
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        sliced_od[rowNum, ]$hospitalname
}

