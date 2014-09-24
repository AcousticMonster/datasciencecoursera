best <- function(state, outcome) {
        
                
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Slice data and Rename Columns
        sliced_od <- outcomedata[c(2, 7, 11, 17, 23)]
        names(sliced_od) <- c("hospitalname", "state", "heart attack", "heart failure", "pneumonia")
        
        ## ------------------------------------------------------------------
        
        ## Check that state and outcome are valid
        ## 1. Test the outcome input (stop if invalid)
        check_outcome <- c("heart attack", "heart failure", "pneumonia")
        if( outcome %in% check_outcome == FALSE ) stop("invalid outcome")
        
        ## 2. Test the state input (stop if invalid)
        states <- sliced_od[, 2]
        states <- unique(states)
        if( state %in% states == FALSE ) stop("invalid state")
        
        ## ------------------------------------------------------------------
        
        ## Return hospital name in that state with lowest 30-day death rate        
        sliced_od <- sliced_od[sliced_od$state == state & sliced_od[outcome] != 'Not Available', ]
        selected_od_val <- sliced_od[, outcome]
        rowNumber <- which.min(selected_od_val)        
        
        
        ## Return hospital name in that state with lowest 30-day death rate
        sliced_od[rowNumber, ]$hospitalname

}


