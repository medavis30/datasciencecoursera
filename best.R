best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
    choice_outcome <- c("heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if((state %in% outcome_data[, 7]) == FALSE){
        
        stop("invalid state")
        
    }
    
    if((outcome %in% choice_outcome) == FALSE){
        
        stop("invalid outcome")
        
    }
    
    ## Pull out rows which satisfy the "state" input
    my_state <- subset(outcome_data,outcome_data[, 7] == state)
    
    ## Use the appropriate column which satisfies the "outcome" input
    if(outcome == "heart attack"){
        
        my_outcome <- as.numeric(my_state[, 11])
        
    }else if(outcome == "heart failure"){
        
        my_outcome <- as.numeric(my_state[, 17])
    
    }else{
        
        my_outcome <- as.numeric(my_state[, 23])
        
    }
    
    ## Remove NAs
    complete <- complete.cases(my_outcome)
    
    clean_outcome <- my_state[complete, ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    if(outcome == "heart attack"){
        
        select_outcome <- as.numeric(clean_outcome[, 11])
        
    }else if(outcome == "heart failure"){
        
        select_outcome <- as.numeric(clean_outcome[, 17])
        
    }else{
        
        select_outcome <- as.numeric(clean_outcome[, 23])
        
    }
    
    lowest_rate <- which(select_outcome == min(select_outcome))
    
    best_hospital <- clean_outcome[lowest_rate, 2]
    
    if(length(best_hospital) > 1){
        
        tie_breaker <- sort(best_hospital)
        tie_breaker[1]
    
    }else{
        
        best_hospital
        
    }
}